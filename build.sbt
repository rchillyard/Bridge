organization := "com.phasmidsoftware"

name := "Bridge"

version := "1.1.0"

scalaVersion := "3.7.4"

val scalaTestVersion = "3.2.20"

scalacOptions ++= Seq(
  "-deprecation",
  "-feature"
)

javacOptions ++= Seq("-source", "23", "-target", "23")

resolvers += Resolver.mavenLocal

lazy val versionFlog   = "1.0.13"
lazy val versionNumber = "1.10.5"

libraryDependencies ++= Seq(
  "com.phasmidsoftware"        %% "flog"                     % versionFlog,
  "com.phasmidsoftware"        %% "number"                   % versionNumber,
  "com.phasmidsoftware"         % "decisiontree_2.13"        % "1.0.5-SNAPSHOT"
    excludeAll(
    ExclusionRule("org.scala-lang.modules",    "scala-parser-combinators_2.13"),
    ExclusionRule("com.typesafe.scala-logging", "scala-logging_2.13"),
    ExclusionRule("com.phasmidsoftware",        "flog_2.13")
  ),
  "com.typesafe.play"          %% "play-json"                % "2.10.8",
  "com.typesafe.scala-logging" %% "scala-logging"            % "3.9.6",
  "org.scala-lang.modules"     %% "scala-xml"                % "2.4.0",
  "org.scala-lang.modules"     %% "scala-parser-combinators" % "2.4.0",
  "joda-time"                   % "joda-time"                % "2.14.2",
  "org.scalatest"              %% "scalatest"                % scalaTestVersion % Test,
  "ch.qos.logback"              % "logback-classic"          % "1.5.9"          % Runtime
)
