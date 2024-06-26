ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.1"

lazy val root = (project in file("."))
  .settings(
    name := "sepgen"
  )


libraryDependencies ++= Seq(
  "dev.optics" %% "monocle-core"  % "3.2.0",
  "dev.optics" %% "monocle-macro" % "3.2.0",
  "org.typelevel" %% "cats-core" % "2.10.0",
  "org.typelevel" %% "kittens" % "3.3.0",
  "org.wvlet.airframe" %% "airframe-log" % "24.4.3"

)

scalacOptions += "-feature"