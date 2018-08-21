import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "edu.depauw",
      scalaVersion := "2.12.6",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "PFDS",
    libraryDependencies += scalaTest % Test,
    libraryDependencies += scalaCheck % Test
  )
