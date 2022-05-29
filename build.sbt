import scala.Seq

ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.8"
cancelable in Global := true


val ZioVersion = "1.0.14"

lazy val root = (project in file("."))
  .settings(
    name := "fpsnake",
    fork := true,
    mainClass := Some("io.adri.fpsnale.FpSnake"),
      libraryDependencies ++= Seq(
      "org.fusesource.jansi" % "jansi" % "1.8",
      "dev.zio" %% "zio" % ZioVersion,
      "dev.zio" %% "zio-streams" % ZioVersion,
      "dev.zio" %% "zio-test"          % ZioVersion % "test",
      "dev.zio" %% "zio-test-sbt"      % ZioVersion % "test",

    )
  )
