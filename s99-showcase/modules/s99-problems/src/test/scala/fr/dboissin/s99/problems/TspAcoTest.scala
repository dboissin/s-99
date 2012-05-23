package fr.dboissin.s99.problems

import org.scalatest.FunSuite
import fr.dboissin.s99.problems.Tsp._
import akka.actor._
import akka.util.duration._
import akka.pattern.ask
import akka.util.Timeout
import akka.dispatch.Await
import java.util.concurrent.TimeUnit
import scala.util.Random
import scala.concurrent.stm._

class TspAcoTest extends FunSuite {

  implicit val timeout = Timeout(60 seconds)
  lazy val cities = TravellingSalesman.loadFromFile(getClass.getClassLoader.getResource("defi250.csv").getFile)

  test("Calculate neighbors distances") {
    val scoredNeighbors = neighborsDistances(cities)
    assert(scoredNeighbors.length > 0)
  }

  test("Calculate mean distance") {
    val mean = meanDistance(neighborsDistances(cities))
    println("mean distance : %s".format(mean))
    assert(mean > 0)
  }

  test("Init pheromones") {
    val pheromones = initPheromones(neighborsDistances(cities))
    assert(pheromones.length > 0)
  }

  test("the ant guide find first path") {
    val eclaireuse = system.actorOf(Props(
      new AntGuide(neighborsDistances(cities))
    ))
    (eclaireuse ? (cities.head, cities)).mapTo[List[City]]
      .map{res =>
        println(res.size)
       println(">>> %s".format(pathLonger(res)))}
  }


  test("the ant find a path") {
    /*
    val ref = Ref(initPheromones(neighborsDistances(cities)))
    val ant = system.actorOf(Props(
      new Ant(invNeighborsDistances(cities), ref.single, new Random)
    ))
    (ant ? (cities.head, cities)).mapTo[List[City]]
      .map{res =>
        println(res.size)
       println(">>> %s".format(pathLonger(res)))}
    */
  }

  test("ACO") {
    val env = system.actorOf(Props(
      new Environment(cities)
    ))
    (env ? "launchWave").mapTo[List[City]]
      .map{res =>
        println(res.size)
       println(">>> %s".format(pathLonger(res)))}
  Thread.sleep(60000)
  }

  def printMatrix(matrix: Array[Array[Double]]) = {
    for (i <- 0 to 249) {
      for (j <- 0 to 249) {
        print(" " + matrix(i)(j))
      }
      println()
    }
  }

}

