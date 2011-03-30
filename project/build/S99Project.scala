import sbt._

class S99Project(info: ProjectInfo) extends DefaultProject(info) {
    val scalatest = "org.scalatest" % "scalatest" % "1.3"
}