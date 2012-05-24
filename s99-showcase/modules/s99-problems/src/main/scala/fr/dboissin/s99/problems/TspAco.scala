package fr.dboissin.s99.problems

import akka.actor._
import akka.routing._
import akka.pattern.ask
import akka.transactor._
import akka.util.duration._
import akka.util.Timeout
import java.util.concurrent.TimeUnit
import scala.math._
import scala.concurrent.stm._
import java.util.Date
import scala.util.Random
import akka.event.Logging
import scala.sys.process._
import java.io.File

class Environment(cities: List[City], seed: Long = new Date().getTime) extends Transactor {

  val log = new FileProcessLogger(new File("/tmp/tspaco"))
  val ANT_NUMBER = 20
  val scoredNeighbors = Tsp.neighborsDistances(cities)
  val pheromones = Ref(Tsp.initPheromones(scoredNeighbors))
  val router = Tsp.system.actorOf(Props(
    new Ant(Tsp.invNeighborsDistances(cities), pheromones.single, new Random(seed))
  ).withRouter(RoundRobinRouter(ANT_NUMBER)))
  val wavePheromones = Ref(Array.fill(cities.size, cities.size)(0.0))
  val antNb = Ref(ANT_NUMBER)
  val waveNb = Ref(40)
  val best = Ref[(List[City], Double)]((Nil, 100000.0))
  val Q = 0.0001
  val rho = 0.3
  val rnd = new Random(seed)

  def atomically = implicit txn => {
    case res: List[City] =>
      updateWavePheromones(res)
      if (antNb.get == 0) {
        updatePheromones
        log.out("Path length : " + best.get._2)
        log.flush()
        if (waveNb.get > 0) {
          launch
        } else {
          println("Best path : " + best.get._2 + " - seed : " + seed)
        }
      }

    case "launchWave" => launch

  }

  private def launch {
    (1 to ANT_NUMBER).foreach{i =>
        router ! (cities.find(_.id == rnd.nextInt(cities.size)).getOrElse(cities.head), cities)
  }}

  private def updateWavePheromones(path: List[City])(implicit txn: InTxn) {
    println("updateWavePheromones - ant restantes : " + antNb.get)
    val pathLength = Tsp.pathLonger(path)
    if (pathLength < best.get._2) {
      best.set((path, pathLength))
    }
    wavePheromones.transform{ p =>
      val size = p.length
      val wp = Array.ofDim[Double](size, size)
      path./:(path.last){(c1, c2) =>
        val pher = (Q / pathLength) + p(c1.id)(c2.id)
        wp(c1.id)(c2.id) = pher
        wp(c2.id)(c1.id) = pher
        c2
      }
      wp
    }
    antNb.transform(_ - 1)
  }

  private def updatePheromones(implicit txn: InTxn) {
    println("updatePheromones - wave : " + waveNb.get)
    val wp = wavePheromones.swap(Array.fill(cities.size, cities.size)(0.0))
    pheromones.transform{ p =>
      val size = p.length
      val tmp = Array.ofDim[Double](size, size)
      for (i <- 0 to (size - 1)) {
        for (j <- (i + 1) to (size - 1)) {
          val pher = ((1 - rho) * p(i)(j)) + wp(i)(j)
          tmp(i)(j) = pher
          tmp(j)(i) = pher
        }
      }
      tmp
    }
    waveNb.transform(_ - 1)
    antNb.set(ANT_NUMBER)
  }

}

trait Routable {

  def findPath(start: City, cities: List[City]): List[City] = {
    cities.filterNot(_.id == start.id)./:(List(start))((acc, _) =>
      nextCity(acc.head, cities.diff(acc))
      .map(_::acc).getOrElse(acc)
    )
  }

  def nextCity(current: City, available: List[City]): Option[City]
//(selectionFunction:  (c, alc) => Option[c])
}

class AntGuide(distances: Array[Array[Double]]) extends Actor with Routable {

  def receive = {
    case (start: City, cities: List[City]) => sender ! findPath(start, cities)
  }

  def nextCity(current: City, available: List[City]) = available match {
    case Nil => None
    case c::Nil => Some(c)
    case l => Some(
      l.map(c =>
        (c, distances(current.id)(c.id))
      ).minBy(_._2)._1
    )
  }

}

class Ant(invDistances: Array[Array[Double]], pheromones: Ref.View[Array[Array[Double]]], rnd: Random) extends Actor with Routable {

  val alpha = -0.2
  val beta = 9.6

  def receive = {
    case (start: City, cities: List[City]) =>
      val path = twoOpt(findPath(start, cities))
      println("path size : " + Tsp.pathLonger(path))
      sender ! path
  }

  def nextCity(current: City, available: List[City]) = available match {
    case Nil => None
    case c::Nil => Some(c)
    case l =>
      val p = pheromones.get
      val probability = l.map(c =>
        (c, pow(p(current.id)(c.id), alpha) * pow(invDistances(current.id)(c.id), beta))
      )
      val probSum = probability./:(0d)((acc, c) => acc + c._2)
      val thresh = rnd.nextDouble * probSum
      probability./:[(Option[City], Double)]((None, 0d))((acc, c) =>
        if (acc._2 > thresh) {
          acc
        } else {
          (Some(c._1), acc._2 + c._2)
        }
      )._1
  }

  private def twoOpt(path: List[City]) = {
    var wait = 100
    var best = path
    var bestLonger = Tsp.pathLonger(best)
    while (wait > 0) {
      val t1 = best.splitAt(rnd.nextInt(best.size))
      val t2 = t1._2.splitAt(rnd.nextInt(t1._2.size))
      val tmp = t1._1 ::: t2._1.reverse ::: t2._2
      val tmpLonger = Tsp.pathLonger(tmp)
      if (tmpLonger < bestLonger) {
        best = tmp
        bestLonger = tmpLonger
        wait = 9000
      } else {
        wait -= 1
      }
    }
    best
  }

}

object Tsp {

  val system = ActorSystem("TspAcoTest")

  def neighborsDistances(cities: List[City]): Array[Array[Double]] = {
    val size = cities.size
    val distances = Array.ofDim[Double](size, size)
    cities.foreach{c1 =>
      cities.foreach{c2 =>
        distances(c1.id)(c2.id) = distanceBetweenCities(c1, c2)
      }
    }
    distances
  }

  def invNeighborsDistances(cities: List[City]): Array[Array[Double]] = {
    val size = cities.size
    val distances = Array.ofDim[Double](size, size)
    cities.foreach{c1 =>
      cities.foreach{c2 =>
        distances(c1.id)(c2.id) = 1 / distanceBetweenCities(c1, c2)
      }
    }
    distances
  }

  def distanceBetweenCities(c1:City, c2: City) = {
    sqrt(pow(c2.x - c1.x, 2) + pow(c2.y - c1.y, 2))
  }

  def meanDistance(neighborsDistances: Array[Array[Double]]): Double = {
    val distances = (
      for (i <- 0 to (neighborsDistances.length - 1))
      yield for (j <- (i + 1) to (neighborsDistances(i).length - 1))
      yield (neighborsDistances(i)(j))
    ).flatten
    distances.reduceLeft(_+_)/distances.size
  }

  def initPheromones(neighborsDistance: Array[Array[Double]]): Array[Array[Double]] = {
    val size = neighborsDistance.length
    val initPheromoneValue = 1 / meanDistance(neighborsDistance)
    Array.fill(size, size)(initPheromoneValue)
  }

  def pathLonger(path: List[City]) =
    path.tail.foldLeft((path.head, distanceBetweenCities(path.last, path.head)))((l, c) =>
      (c, l._2 + distanceBetweenCities(l._1, c)))._2

  def printMatrix(matrix: Array[Array[Double]]) = {
    for (i <- 0 to 249) {
      for (j <- 0 to 249) {
        print(" " + matrix(i)(j))
      }
      println()
    }
  }
}

