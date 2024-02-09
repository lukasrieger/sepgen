ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.1"

lazy val root = (project in file("."))
  .settings(
    name := "sepgen"
  )


libraryDependencies ++= Seq(
  "dev.optics" %% "monocle-core"  % "3.2.0",
  "dev.optics" %% "monocle-macro" % "3.2.0",
)