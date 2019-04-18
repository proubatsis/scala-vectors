import Dependencies._

ThisBuild / scalaVersion     := "2.12.8"
ThisBuild / version          := "1.0.0"
ThisBuild / organization     := "io.github.proubatsis"
ThisBuild / organizationName := "proubatsis"
ThisBuild / homepage := Some(url("https://github.com/proubatsis/scala-vectors"))

ThisBuild / scmInfo := Some(ScmInfo(
  url("https://github.com/proubatsis/scala-vectors"),
  "https://github.com/proubatsis/scala-vectors.git"))

ThisBuild / developers := List(Developer(
  "proubatsis",
  "Panagiotis Roubatsis",
  "scala-vectors@proubatsis.com",
  url("https://github.com/proubatsis")))

ThisBuild / licenses := List("MIT" -> url("https://opensource.org/licenses/MIT"))
ThisBuild / publishMavenStyle := true

ThisBuild / publishTo := Some(
  if (isSnapshot.value) Opts.resolver.sonatypeSnapshots
  else Opts.resolver.sonatypeStaging
)

ThisBuild / pomIncludeRepository := { _ => false }
ThisBuild / useGpg := true

lazy val root = (project in file("."))
  .settings(
    name := "Scala Vectors",
    libraryDependencies += scalaTest % Test
  )

credentials += Credentials(Path.userHome / ".sbt" / "sonatype_credential")

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
