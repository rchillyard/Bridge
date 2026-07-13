organization := "com.phasmidsoftware"

name := "Bridge"

version := "1.1.3-SNAPSHOT"

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
lazy val versionGambit = "1.2.2-SNAPSHOT"
val versionLogback     = "1.5.38"

libraryDependencies ++= Seq(
  "com.phasmidsoftware"        %% "flog"                     % versionFlog,
  "com.phasmidsoftware"        %% "number"                   % versionNumber,
  "com.phasmidsoftware"        %% "gambit"                   % versionGambit,
  "com.typesafe"                 % "config"                  % "1.4.1",
  "com.typesafe.play"          %% "play-json"                % "2.10.8",
  "com.typesafe.scala-logging" %% "scala-logging"            % "3.9.6",
  "org.scala-lang.modules"     %% "scala-xml"                % "2.4.0",
  "org.scala-lang.modules"     %% "scala-parser-combinators" % "2.4.0",
  "joda-time"                   % "joda-time"                % "2.14.2",
  "org.scalatest"              %% "scalatest"                % scalaTestVersion % Test,
  "ch.qos.logback"              % "logback-classic"          % versionLogback          % Runtime
)

lazy val IT = config("it") extend Test

// `Test`/`IT`/`run` all fork a separate JVM (below), so a `-Dbridge....=...` passed to the
// outer `sbt` command never reaches it on its own -- forward it explicitly. This lets e.g.
// `sbt -Dbridge.transposition-table.max-size=50000 IT/test` actually take effect.
lazy val forwardedBridgeProps: Seq[String] =
  sys.props.collect { case (k, v) if k.startsWith("bridge.") => s"-D$k=$v" }.toSeq

lazy val root = project.in(file("."))
  .configs(IT)
  .settings(
    inConfig(IT)(Defaults.testSettings),
    IT / scalaSource := baseDirectory.value / "src" / "it" / "scala",
    IT / fork := true,
    IT / javaOptions ++= Seq("-Xms512m", "-Xmx8g") ++ forwardedBridgeProps
  )

// NOTE: the following does not seem to work.
run / javaOptions ++= Seq("-Xms512m", "-Xmx8g") ++ forwardedBridgeProps
run / fork := true
Test / javaOptions ++= Seq("-Xms512m", "-Xmx8g") ++ forwardedBridgeProps
Test / fork := true