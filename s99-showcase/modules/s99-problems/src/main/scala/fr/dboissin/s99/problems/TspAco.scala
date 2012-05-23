package fr.dboissin.s99.problems

import akka.actor._
import akka.routing._
import akka.pattern.ask
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

class Environment(cities: List[City], seed: Long = new Date().getTime) extends Actor {

  val log = new FileProcessLogger(new File("/tmp/tspaco"))
  val scoredNeighbors = Tsp.neighborsDistances(cities)
  val pheromones = Ref(Tsp.initPheromones(scoredNeighbors))
  val router = Tsp.system.actorOf(Props(new Ant(Tsp.invNeighborsDistances(cities), pheromones.single, new Random(seed))).withRouter(RoundRobinRouter(20)))
  val wavePheromones = Ref(Array.fill(cities.size, cities.size)(0.0))
  val antNb = Ref(19)
  val waveNb = Ref(39)
  val best = Ref[(List[City], Double)]((Nil, 10000.0))
  val Q = 4.0
  val rho = 0.1
  val rnd = new Random(seed)

  def receive = {
    case res: List[City] =>
      updateWavePheromones(res)
      if (antNb.single.get == 0) {
        updatePheromones
        log.out("Path length : " + best.single.get._2)
        log.flush()
        if (waveNb.single.get > 0) {
          self ! "launchWave"
        } else {
          println("Best path : " + best.single.get._2)
          Tsp.printMatrix(pheromones.single.get)
        }
      }

    case "launchWave" =>
      (1 to 20).foreach{i =>
        router ! (cities.find(_.id == rnd.nextInt(cities.size)).getOrElse(cities.head), cities)
          //.mapTo[List[City]].map(res => self ! res)
      }

  }

  // TODO rendre cette méthode transactionnelle
  private def updateWavePheromones(path: List[City]) {
    println("updateWavePheromones - ant restantes : " + antNb.single.get)
    val pathLength = Tsp.pathLonger(path)
    if (pathLength < best.single.get._2) {
      best.single.set((path, pathLength))
    }
    wavePheromones.single.transform{ p =>
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
    antNb.single.transform(_ - 1)
  }

  // TODO rendre cette méthode transactionnelle
  private def updatePheromones {
    println("updatePheromones - wave : " + waveNb.single.get)
    val wp = wavePheromones.single.swap(Array.fill(cities.size, cities.size)(0.0))
    pheromones.single.transform{ p =>
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
    waveNb.single.transform(_ - 1)
    antNb.single.set(19)
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

  val alpha = 0.1
  val beta = 2.0

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
        wait = 1000
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

