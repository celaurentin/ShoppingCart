
ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.8"

lazy val ShoppingCart = (project in file("."))
  .settings(
    libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.13" % "test",
    libraryDependencies += "org.scalatest" %% "scalatest-funspec" % "3.2.13" % "test",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.13" % "test"
  )
