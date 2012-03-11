package actors

import akka.actor._
import akka.routing._
import play.api._
import play.api.libs.iteratee._
import play.api.libs.concurrent._
import play.api.libs.Codecs._
import fr.dboissin.s99.problems._
import scala.collection.mutable._

case class GetPath(hash: String, lastPathSize: Option[Double] = None)

class TravellingSalesmanManagement(poolSize:Int = 20) extends Actor {
  val router = context.actorOf(Props[TravellingSalesman].withRouter(RoundRobinRouter(poolSize)))
  val futures = new HashMap[String, List[PushEnumerator[SearchResult]]]
  val paths = new HashMap[String, SearchResult]

  def receive = {
    case SearchPath(cities, _, seed) =>
      val hash = sha1(cities.toString)
      if (!paths.get(hash).isDefined) {
        paths.put(hash, null)
        router ! Start(hash, cities, seed)
      }
      sender ! hash
    case GetPath(hash, last) =>
      val sf = Enumerator.imperative[SearchResult]( onStart = self ! hash)
      sender ! sf
      paths.get(hash) match {
        case Some(path) if (path != null && (!last.isDefined || last.get > path.pathSize)) =>
          sf.push(path)
        case _ => Logger.error("Path not found!")
      }
      futures.get(hash) match {
         case Some(l) => futures.update(hash, sf :: l)
         case None => futures.put(hash, List(sf))
       }
    case Result(hash, individual, seed) =>
      Logger.debug("%s, %s, %s".format(hash, individual.pathSize, seed))
      val res = SearchResult(individual.path, individual.pathSize, seed)
      val up = paths.get(hash) match {
        case None =>
          paths.put(hash, res)
          true
        case Some(path) if (path == null || path.pathSize > individual.pathSize) =>
          paths.update(hash, res)
          true
        case _ => false
      }
      if (up && futures.get(hash).isDefined) {
        futures.get(hash).get.foreach(_.push(res))
      }
  }

}


object Actors {
  lazy val system = ActorSystem("TravellingSalesman")
  lazy val travellingSalesman = system.actorOf(Props(new TravellingSalesmanManagement()))
}
