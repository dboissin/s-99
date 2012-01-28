import sbt._
import Keys._
import PlayProject._


object ApplicationBuild extends Build {

    val appName         = "s99-showcase"
    val appVersion      = "0.1"

    val appDependencies = Seq(
      "fr.dboissin.s99" % "s99-problems" % "0.1-SNAPSHOT"
    )


    val main = PlayProject(appName, appVersion, appDependencies, mainLang = SCALA).settings()

}
