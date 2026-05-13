name := "Bridge"

version := "1.1.0"

scalaVersion := "3.7.4"

val scalaTestVersion = "3.2.19"

resolvers += Resolver.mavenLocal
resolvers += "Typesafe Repository" at "https://repo.typesafe.com/typesafe/releases"

lazy val versionFlog = "1.0.13"

libraryDependencies ++= Seq(
  "com.phasmidsoftware"        %% "flog"             % versionFlog,
  "org.scala-lang.modules" %% "scala-xml" % "2.3.0",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5",  // already fine
  "com.typesafe.play" %% "play-json" % "2.10.8",
  "com.phasmidsoftware" % "decisiontree_2.13" % "1.0.5-SNAPSHOT"
    excludeAll(
    ExclusionRule("org.scala-lang.modules", "scala-parser-combinators_2.13"),
    ExclusionRule("com.typesafe.scala-logging", "scala-logging_2.13"),
    ExclusionRule("com.phasmidsoftware", "flog_2.13")
  ),
  "com.phasmidsoftware" % "number_2.13" % "1.1.1-SNAPSHOT"
    excludeAll(
    ExclusionRule("org.scala-lang.modules", "scala-parser-combinators_2.13"),
    ExclusionRule("com.typesafe.scala-logging", "scala-logging_2.13"),
    ExclusionRule("com.phasmidsoftware", "flog_2.13")
  ),
  "joda-time" % "joda-time" % "2.12.7",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "2.4.0",
  "org.scalatest" %% "scalatest" % scalaTestVersion % "test",
  "ch.qos.logback" % "logback-classic" % "1.5.9" % "runtime"
)
