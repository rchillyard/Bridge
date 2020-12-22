name := "Bridge"

version := "1.0.2-SNAPSHOT"

scalaVersion := "2.13.4"

val scalaTestVersion = "3.1.1"

scalacOptions ++= Seq("-deprecation", "-feature")

resolvers += "Typesafe Repository" at "https://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= Seq(
  "com.phasmidsoftware" %% "decisiontree" % "1.0.4",
  "joda-time" % "joda-time" % "2.9.2",
  "org.scala-lang.modules" %% "scala-xml" % "1.3.0",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2",
  "org.scalatest" %% "scalatest" % scalaTestVersion % "test",
  "ch.qos.logback" % "logback-classic" % "1.2.3" % "runtime",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2"
)

val sprayGroup = "io.spray"
val sprayJsonVersion = "1.3.6"
libraryDependencies ++= List("spray-json") map {c => sprayGroup %% c % sprayJsonVersion}

//wartremoverErrors ++= Warts.unsafe
