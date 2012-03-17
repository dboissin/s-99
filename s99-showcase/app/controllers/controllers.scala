package controllers

import play.api._
import actors._
import akka.util.Timeout
import akka.util.duration._
import akka.actor.Actor._
import akka.pattern.ask
import play.api.mvc.Results._
import play.api.mvc._
import play.api.libs.iteratee.Enumerator
import fr.dboissin.s99.problems._
import play.api.libs._
import play.api.libs.iteratee._
import play.api.libs.Comet.CometMessage
import play.api.libs.concurrent._
import play.api.libs.json._
import Json._

object Showcase extends Controller {

  implicit val timeout = Timeout(5 seconds)

  implicit object CityFormat extends Writes[City] {
    def writes(c: City): JsValue = JsObject(List(
      "id" -> JsNumber(c.id),
      "x" -> JsNumber(c.x),
      "y" -> JsNumber(c.y)))
  }

  implicit object SearchResultFormat extends Writes[SearchResult] {
    def writes(sr: SearchResult): JsValue = JsObject(List(
      "path" -> toJson(sr.path),
      "distance" -> JsNumber(sr.pathSize),
      "seed" -> JsNumber(sr.seed)))
  }

  implicit object Tuple2IntFormat extends Writes[(Int, Int)] {
    def writes(t: (Int, Int)): JsValue = JsArray(List(
      JsNumber(t._1), JsNumber(t._2)))
  }

  implicit val searchResultMessage = CometMessage[SearchResult](toJson(_).toString)

  def list = Action { implicit request =>
    val testList = List(("knighttour", "Knight's tour"),
        ("travellingsalesman", "Travelling Salesman"))
    Ok(views.html.list(testList))
  }

  def knightTour(size: Int, x:Int, y: Int) = Action { implicit request =>
    val jsonPath = KnightTour.findPath((x,y), size) match {
    case Some(path) => Some(toJson(path).toString)
    case _ => None
    }
    Ok(views.html.knighttour(jsonPath, size))
  }

  def travellingSalesman = Action { implicit request =>
    AsyncResult {
      val cities = Mock.cities
      (Actors.travellingSalesman ? SearchPath(cities)).mapTo[InitInfos]
        .asPromise.map{ init =>
          Logger.info("Init : %s - %s".format(init.hash, init.path.map(_.pathSize).getOrElse("")))
          Ok(views.html.travellingsalesman(init.hash, init.path.map(toJson(_).toString)))
        }
    }
  }

  def travellingSalesmanSearch(hash:String) = Action {
    AsyncResult {
      (Actors.travellingSalesman ? GetPath(hash))
        .mapTo[Enumerator[SearchResult]].asPromise.map(
          chunks => Ok.stream(chunks &> Comet(callback = "parent.callback"))
        )
    }
  }

}
