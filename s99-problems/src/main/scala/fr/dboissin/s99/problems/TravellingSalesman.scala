package fr.dboissin.s99.problems

import scala.math._
import scala.collection.immutable.StringOps
import scala.util.Random
import java.util.Date
import scala.actors.Actor

case class City(id:Int, x:Double, y:Double)

case class Individual(path:List[City], pathSize: Double)

class PartialResult extends Actor {
  def act() {
    loop {
      react {
        case Individual(path, pathSize) => println("Distance : " + pathSize + " - " + path)
      }
    }
  }
}

class TravellingSalesmanGA (
  val initCities: List[City],
  val partialRes: Actor = null,
  val seed: Long = new Date().getTime,
  val populationSize:Int = 20,
  val selectionSize:Int = 5,
  val waitIdx:Int = 9999999
) extends Actor {
  import fr.dboissin.s99.problems.TravellingSalesmanHelper._

  val random: Random = new Random(seed)
  var best:Individual = null

  def generatePopulation(cities: List[City]) = {
    List.fill(populationSize)(abs(random.nextInt%cities.size))
    .map(startCity => {
      val path = greedy(cities, cities.apply(startCity))
      Individual(path, pathLonger(path))
    })
  }

  def selection(population:List[Individual]) = {
    val selected = List.fill(selectionSize)(abs(random.nextInt%population.size))
    .map(idx => population.apply(idx))
    .sortWith((ind1, ind2) => ind1.pathSize < ind2.pathSize)
    val idx = population.indexWhere(ind => ind.pathSize == selected.last.pathSize)
    val tmp = population.splitAt(idx)
    selected.head::tmp._1:::tmp._2.tail
  }

  def act() {
    var population = generatePopulation(initCities)
    var wait = waitIdx
    best = population.head
    while (wait > 0) {
      population = selection(population)
      population = twoOpt(population.head, abs(random.nextInt), abs(random.nextInt))::population.tail
      val tmp = bestIndividual(population)
      if (tmp.pathSize < best.pathSize) {
        best = tmp
        wait = waitIdx
        if (partialRes != null) {
          partialRes ! tmp
        }
      } else {
        wait -= 1
      }
    }
  }

  override def toString = "Seed : " + seed + " - Distance : " + best.pathSize + " Solution : " + best.path

}

object TravellingSalesmanHelper {

  import scala.io.Source._

  def greedy(cities:List[City], start: City): List[City] = cities match {
    case city::Nil => city::Nil
    case _ =>
      val nextCity = cities.tail.foldLeft((cities.head,
          distanceBetweenCities(start, cities.head)))((c,cur) => {
        val d = distanceBetweenCities(start, cur)
	    if (d < c._2) 
	   	  (cur, d)
	    else
	      c
	  })._1
	  nextCity::greedy(cities.filter(e => e != nextCity), nextCity)
  }

  /**
   * @return the distance between two cities.
   */
  def distanceBetweenCities(c1:City, c2: City) = {
    sqrt(pow(c2.x - c1.x, 2) + pow(c2.y - c1.y, 2))
  }

  def loadFromFile(path: String): List[City] = {
    implicit def string2Double(s: String): Double = new StringOps(s).toDouble
    
    val lines = fromFile(path).getLines().toList;
    var i = -1
    lines.tail.map(line => { 
      val coordinates = line.split(";")
      i += 1
      City(i, coordinates(0), coordinates(1))
    }) 
  }

  def pathLonger(path: List[City]) = 
    path.tail.foldLeft((path.head, distanceBetweenCities(path.last, path.head)))((l, c) =>
      (c, l._2 + distanceBetweenCities(l._1, c)))._2

  def twoOpt(ind: Individual, idx1: Int, idx2: Int) = {
    val listSize = ind.path.size
    val range =
      if (idx1 < idx2) {
        (idx1%listSize, idx2%listSize)
      } else {
        (idx2%listSize, idx1%listSize)
      }
    val tmp = ind.path.splitAt(range._1)
    val tmp2 = tmp._2.splitAt(range._2)
    val newPath = tmp._1:::tmp2._1.reverse:::tmp2._2
    Individual(newPath, pathLonger(newPath))
  }

  def bestIndividual(population: List[Individual]) = {
	population.tail.foldLeft(population.head)((best, current) =>
	  (if (current.pathSize < best.pathSize) current else best))
  }

}
