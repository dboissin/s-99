package actors

import akka.actor.Actor._
import akka.routing.Routing
import fr.dboissin.s99.problems.SearchPath
import akka.routing.CyclicIterator
import akka.routing.Routing.Broadcast
import fr.dboissin.s99.problems.SearchResult
import scala.collection.mutable.HashMap
import fr.dboissin.s99.problems.Result
import fr.dboissin.s99.problems.Hash
import scala.collection.mutable.ListBuffer
import fr.dboissin.s99.problems.TravellingSalesman
import fr.dboissin.s99.problems.Start
import akka.actor._
import play.api._
import play.api.libs.iteratee._
import play.api.libs.concurrent._

case class GetPath(hash:String)

class TravellingSalesmanManagement(poolSize:Int = 20) extends Actor {

  val futures = new HashMap[String, ListBuffer[CallbackEnumerator[SearchResult]]]
  val bests = new HashMap[String, SearchResult]
  val workers = Vector.fill(poolSize)(actorOf[TravellingSalesman].start())
  val loadBalancer = Routing.loadBalancerActor(CyclicIterator(workers))

  private def addFuture(key:String, future:CallbackEnumerator[SearchResult]) = {
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
    case GetPath(hash) =>
      lazy val sf:CallbackEnumerator[SearchResult] = new CallbackEnumerator[SearchResult] (
          onComplete = {
           // Logger.info("Disconnected : " + sf)
            futures.get(hash).foreach(_.filterNot(_ == sf))
          }
      )
      bests.get(hash) match {
        case Some(res) =>
          addFuture(hash, sf)
          sf.push(res)
        case None =>
          addFuture(hash, sf)
      }
      self.reply(sf)

    case SearchPath(cities, _, seed) =>
      val hash = Hash.sha1(cities.toString)
      println(hash)
      if (!bests.contains(hash)) {
        loadBalancer ! Start(hash, cities, seed)
      }
      self.senderFuture.map(_.completeWithResult(hash)) //.getOrElse(throw new RuntimeException("Response Error"))

    case Result(id, individual, seed) =>
      val res = SearchResult(individual.path, individual.pathSize, seed)
      addResult(id, res)
      futures.get(id).foreach{lf =>
        lf.foreach(f => f.push(res))
      }
  }

  override def postStop() = {
    loadBalancer ! Broadcast(PoisonPill)
    loadBalancer ! PoisonPill
  }

}

object Actors {
  lazy val travellingSalesman = actorOf(new TravellingSalesmanManagement())
}
