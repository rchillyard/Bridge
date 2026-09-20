organization := "com.phasmidsoftware"

name := "Bridge"

version := "1.1.6"

scalaVersion := "3.7.4"

val scalaTestVersion = "3.2.20"

scalacOptions ++= Seq(
  "-deprecation",
  "-feature"
)

javacOptions ++= Seq("-source", "23", "-target", "23")

resolvers += Resolver.mavenLocal

lazy val versionFlog    = "1.0.15"
lazy val versionNumber  = "1.10.5"
lazy val versionGambit  = "1.2.6"
lazy val versionLogback = "1.5.38"
lazy val versionConfig  = "1.4.9"

libraryDependencies ++= Seq(
  "com.phasmidsoftware"        %% "flog"                     % versionFlog,
  "com.phasmidsoftware"        %% "number"                   % versionNumber,
  "com.phasmidsoftware"        %% "gambit"                   % versionGambit,
  "com.typesafe"                % "config"                   % versionConfig,
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

// CircleCI sets CIRCLECI=true automatically. The forked Test/IT JVM's heap request needs to
// leave room alongside the sbt launcher's own JVM (capped separately via CircleCI's own
// JVM_OPTS) inside whatever the container's resource_class actually provides -- asking for 8g
// in an 8g (or worse, the 4g default) container starves the launcher and thrashes rather than
// failing cleanly, which is exactly what caused a real job to sit silent for 10+ minutes and
// get killed (2026-07-21). 4g leaves comfortable headroom even on a `large` (8g) container.
lazy val forkedHeapXmx: String = if (sys.env.contains("CIRCLECI")) "4g" else "8g"

lazy val root = project.in(file("."))
  .configs(IT)
  .settings(
    inConfig(IT)(Defaults.testSettings),
    IT / scalaSource := baseDirectory.value / "src" / "it" / "scala",
    IT / fork := true,
    IT / javaOptions ++= Seq("-Xms512m", s"-Xmx$forkedHeapXmx") ++ forwardedBridgeProps
  )

// NOTE: the following does not seem to work.
run / javaOptions ++= Seq("-Xms512m", "-Xmx8g") ++ forwardedBridgeProps
run / fork := true
Test / javaOptions ++= Seq("-Xms512m", s"-Xmx$forkedHeapXmx") ++ forwardedBridgeProps
Test / fork := true