import sbt._
import Keys._
import PlayProject._


object ApplicationBuild extends Build {

    val appName         = "s99-showcase"
    val appVersion      = "0.1"

    val appDependencies = Seq(
    )

    val s99Problems = Project("s99-problems", file("modules/s99-problems"))

    val main = PlayProject(
      appName, appVersion, appDependencies, mainLang = SCALA
    ).dependsOn(s99Problems)

}

