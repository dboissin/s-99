resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

organization := "fr.dboissin.s99"

name := "s99-problems"

version := "0.1-SNAPSHOT"

libraryDependencies ++= Seq(
  "com.typesafe.akka" % "akka-actor" % "2.0",
  "com.typesafe.akka" % "akka-transactor" % "2.0",
  "org.scala-tools" %% "scala-stm" % "0.4",
  "org.scalatest" %% "scalatest" % "1.6.1" % "test",
  "junit" % "junit" % "4.8.1" % "test"
)
