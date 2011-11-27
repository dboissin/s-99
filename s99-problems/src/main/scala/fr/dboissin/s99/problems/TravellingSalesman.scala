package fr.dboissin.s99.problems

import java.security.MessageDigest
import java.util.Date

import scala.collection.immutable.StringOps
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import scala.math._
import scala.util.Random

import akka.actor.Actor._
import akka.actor.Actor
import akka.actor.PoisonPill
import akka.dispatch.CompletableFuture
import akka.routing.Routing.Broadcast
import akka.routing.CyclicIterator
import akka.routing.Routing

case class City(id:Int, x:Double, y:Double)
case class Start(id:String, initCities: List[City], seed: Long = new Date().getTime,
    populationSize:Int = 20, selectionSize:Int = 5, waitIdx:Int = 9999999)
case class Individual(path:List[City], pathSize: Double)
case class Result(id:String, individual:Individual, seed: Long)
case class SearchPath(cities: List[City], lastDistance: Option[Double]=None,
    seed: Long = new Date().getTime)
case class SearchResult(path: List[City], pathSize: Double, seed: Long)

class TravellingSalesmanManagement(poolSize:Int = 20) extends Actor {

  val futures = new HashMap[String, ListBuffer[CompletableFuture[Any]]]
  val bests = new HashMap[String, SearchResult]
  val workers = Vector.fill(poolSize)(actorOf[TravellingSalesman].start())
  val loadBalancer = Routing.loadBalancerActor(CyclicIterator(workers))

  private def addFuture(key:String, future:CompletableFuture[Any]) = {
    futures.get(key) match {
      case Some(futures) =>
        futures += future
        false
      case None =>
        futures.put(key, ListBuffer.apply(future))
        true
    }
  }

  private def addResult(key:String, result:SearchResult) = {
    bests.remove(key) match {
      case Some(res) =>
        if (result.pathSize < res.pathSize) {
          bests.put(key, result)
        } else {
          bests.put(key, res)
        }
      case None => bests.put(key, result)
    }
  }

  def receive = {
    case SearchPath(cities, lastDistance, seed) =>
      val hash = Hash.sha1(cities.toString)
      val sf = self.senderFuture.getOrElse(throw new RuntimeException("Response Error"))

      bests.get(hash) match {
        case Some(res) =>
          if (lastDistance.isDefined && res.pathSize < lastDistance.get) {
            sf.completeWithResult(res)
          } else {
            addFuture(hash, sf)
          }
        case None =>
          if (addFuture(hash, sf)) {
            loadBalancer ! Start(hash, cities, seed)
          }
      }

    case Result(id, individual, seed) =>
      val res = SearchResult(individual.path, individual.pathSize, seed)
      addResult(id, res)
      futures.get(id).foreach{lf =>
        lf.foreach(f => f.completeWithResult(res))
      }
  }

    override def postStop() = {
    loadBalancer ! Broadcast(PoisonPill)
    loadBalancer ! PoisonPill
  }

}

class TravellingSalesman extends Actor {
  import fr.dboissin.s99.problems.TravellingSalesman._

  implicit val random = new Random()

  def receive = {
    case Start(id, initCities, seed, populationSize, selectionSize, waitIdx) =>
      random.setSeed(seed)
      var population = generatePopulation(initCities, populationSize)
      var wait = waitIdx
      var best = population.head
      while (wait > 0) {
        population = selection(population, selectionSize)
        population = twoOpt(population.head, abs(random.nextInt),
          abs(random.nextInt))::population.tail
        val tmp = bestIndividual(population)
        if (tmp.pathSize < best.pathSize) {
          best = tmp
          wait = waitIdx
          self.reply(Result(id, best, seed))
        } else {
          wait -= 1
        }
      }
  }
}

object TravellingSalesman {

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

  def generatePopulation(cities: List[City], populationSize:Int)(implicit random: Random) = {
    List.fill(populationSize)(abs(random.nextInt%cities.size))
    .map(startCity => {
      val path = greedy(cities, cities.apply(startCity))
      Individual(path, pathLonger(path))
    })
  }

  def selection(population:List[Individual], selectionSize:Int)(implicit random: Random) = {
    val selected = List.fill(selectionSize)(abs(random.nextInt%population.size))
    .map(idx => population.apply(idx))
    .sortWith((ind1, ind2) => ind1.pathSize < ind2.pathSize)
    val idx = population.indexWhere(ind => ind.pathSize == selected.last.pathSize)
    val tmp = population.splitAt(idx)
    selected.head::tmp._1:::tmp._2.tail
  }
}

object Hash {
  private def bytes2Hex(bytes: Array[Byte]): String = {
    def cvtByte(b: Byte): String = {
      (if (( b & 0xff ) < 0x10 ) "0" else "") + java.lang.Long.toString(b & 0xff, 16)
    }
    bytes.map(cvtByte(_)).mkString.toUpperCase
  }

  def sha1(text: String) = {
    val md = MessageDigest.getInstance("SHA")
    md.reset
    md.update(text.getBytes("UTF-8"), 0, text.length())
    bytes2Hex(md.digest)
  }
}
