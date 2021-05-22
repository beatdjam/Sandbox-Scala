name := "Sandbox-Scala"

version := "0.1"

scalaVersion := "2.13.5"
resolvers += Resolver.sbtPluginRepo("releases")

libraryDependencies ++= Seq(
  "org.json4s" %% "json4s-jackson" % "3.5.5",
  "org.scala-sbt" %% "io" % "1.3.0",
  "org.scalatest" %% "scalatest" % "3.0.8" % Test
)
