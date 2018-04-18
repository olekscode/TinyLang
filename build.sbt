lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "ch.epfl.scala",
      scalaVersion := "2.12.4"
    )),
    name := "tinylang"
  )

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"