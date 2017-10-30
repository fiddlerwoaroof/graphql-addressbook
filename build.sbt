organization := "com.fiddlerwoaroof.experiments"
scalaVersion := "2.12.3"
version      := "0.1.0-SNAPSHOT"

name         := "graphql-addressbook"

scalacOptions += "-Ypartial-unification"

libraryDependencies +=  "org.scalatest" %% "scalatest" % "3.0.3"

libraryDependencies ++= Seq(
  ("sangria",            "1.3.1"),
  ("sangria-circe",      "1.1.0"),
  ("sangria-spray-json", "1.0.0")
).map({case (artifactStr,versionStr) => "org.sangria-graphql" %% artifactStr % versionStr})

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser"
).map(_ % "0.8.0")

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-http" % "10.0.10",
  "com.typesafe.akka" %% "akka-http-spray-json" % "10.0.10"
)

libraryDependencies += "org.typelevel" %% "cats-core" % "1.0.0-MF"

