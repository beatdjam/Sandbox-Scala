ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.8"

lazy val root = (project in file("."))
  .settings(
    name := "A-Written-Interpriter-In-Scala",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % Test,
    scalacOptions += "-deprecation"
  )
