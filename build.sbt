name := "Bridge"

version := "1.0.3-SNAPSHOT"

scalaVersion := "2.13.14"

val scalaTestVersion = "3.2.9"

resolvers += Resolver.mavenLocal
resolvers += "Typesafe Repository" at "https://repo.typesafe.com/typesafe/releases"

resolvers += "Typesafe Repository" at "https://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= Seq(
  "com.typesafe.play" %% "play-json" % "2.10.6",
//  "com.phasmid" %% "lascala" % "1.0.11",
  "com.phasmidsoftware" %% "decisiontree" % "1.0.5-SNAPSHOT",
  "com.phasmidsoftware" %% "number" % "1.1.0",
  "joda-time" % "joda-time" % "2.12.7",
  "org.scala-lang.modules" %% "scala-xml" % "1.3.1",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "2.2.0",
  "org.scalatest" %% "scalatest" % scalaTestVersion % "test",
  "ch.qos.logback" % "logback-classic" % "1.5.8" % "runtime",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5"
)

//val sprayGroup = "io.spray"
//val sprayJsonVersion = "1.3.2"
//libraryDependencies ++= List("spray-json") map {c => sprayGroup %% c % sprayJsonVersion}

//wartremoverErrors ++= Warts.unsafe
