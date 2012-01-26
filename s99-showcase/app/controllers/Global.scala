package controllers

import play.api._
import actors.Actors._

object Global extends GlobalSettings{

  override def onStart(app:Application) {
    Logger.info("Start Showcase")
  }

  override def onStop(app:Application) {
    Logger.info("Stop Showcase")
    system.shutdown
  }

}
