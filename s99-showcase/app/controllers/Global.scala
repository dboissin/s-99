package controllers

import play.api._
import actors.Actors._

object Global extends GlobalSettings{

  override def onStart(app:Application) {
    travellingSalesman.start
  }

  override def onStop(app:Application) {
    travellingSalesman.stop
  }

}
