package controllers

import play.api._
import actors._
import akka.util.duration._
import akka.actor.Actor._
import play.api.mvc.Results._
import play.api.mvc._
import play.api.libs.iteratee.Enumerator
import fr.dboissin.s99.problems._
import play.api.libs._
import play.api.libs.Comet.CometMessage
import play.api.libs.concurrent._
import play.api.libs.json._
import Json._

object Showcase extends Controller {

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
    val cities = Mock.cities
    (Actors.travellingSalesman ? (SearchPath(cities), 5.seconds)).mapTo[String]
        .foreach(Logger.info(_))
    Ok(views.html.travellingsalesman(None))
  }

  def travellingSalesmanSearch(hash:String) = Action {
    AsyncResult {
      (Actors.travellingSalesman ? (GetPath(hash), 5.seconds))
        .mapTo[Enumerator[SearchResult]].asPromise.map(
          chunks => Ok.stream(chunks &> Comet(callback = "parent.callback"))
        )
    }
  }

}
