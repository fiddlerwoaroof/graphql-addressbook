organization := "com.fiddlerwoaroof.experiments"
scalaVersion := "2.12.3"
version      := "0.1.0-SNAPSHOT"

name         := "graphql-addressbook"

libraryDependencies +=  "org.scalatest" %% "scalatest" % "3.0.3"

libraryDependencies += "org.sangria-graphql" %% "sangria" % "1.3.1"
libraryDependencies += "org.sangria-graphql" %% "sangria-circe" % "1.1.0"

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser"
).map(_ % "0.8.0")
