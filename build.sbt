// See README.md for license details.

ThisBuild / scalaVersion     := "2.12.8"
ThisBuild / version          := "0.1.0"
ThisBuild / organization     := "lyn"

val chiselVersion = "3.5.1"

lazy val root = (project in file("."))
  .settings(
    name := "zircon",
    libraryDependencies ++= Seq(
      "edu.berkeley.cs" %% "chisel3" % chiselVersion,
      "edu.berkeley.cs" %% "chisel-iotesters" % "2.5.0",
      "edu.berkeley.cs" %% "rocketchip" % "1.2.6",
      "org.scalatest" %% "scalatest" % "3.0.5",
      "ch.qos.logback" % "logback-classic" % "1.2.10",
      "com.typesafe.scala-logging" %% "scala-logging" % "3.9.4",
      // "edu.berkeley.cs" %% "hardfloat" % "1.2.4",
      "edu.berkeley.cs" %% "chiseltest" % "0.5.0" % "test"
    ),
    scalacOptions ++= Seq(
      "-language:reflectiveCalls",
      "-deprecation",
      "-feature",
      "-Xcheckinit",
      "-P:chiselplugin:genBundleElements",
    ),
    addCompilerPlugin("edu.berkeley.cs" % "chisel3-plugin" % chiselVersion cross CrossVersion.full),
  )

