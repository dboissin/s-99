package fr.dboissin.s99.problems

import scala.math._
import scala.collection.immutable.StringOps

case class City(id:Int, x:Double, y:Double)

class TravellingSalesman {
  // TODO implement a genetic algorithm
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
  
}
