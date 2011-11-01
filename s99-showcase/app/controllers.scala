package controllers

import play.api.Play
import play.api.mvc._
import play.api.mvc.Results._
import sjson.json._
import DefaultProtocol._
import fr.dboissin.s99.problems.KnightTour._

object Showcase extends Controller {

    def list = Action { implicit request =>
        val testList = List(("knighttour", "Knight's tour"))
        println(JsonSerialization.tojson(testList).toString())
        Ok(views.html.list(testList))
    }

    def knightTour(size: Int, x:Int, y: Int) = Action { implicit request =>
      val jsonPath = findPath((x,y), size) match {
        case Some(path) => Some(JsonSerialization.tojson(path).toString)
        case _ => None
      }
      Ok(views.html.knighttour(jsonPath, size))
    }
}
