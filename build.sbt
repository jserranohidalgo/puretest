import xerial.sbt.Sonatype._

lazy val commonSettings = Seq(
  organization := "org.hablapps",
  version := "0.3.2",
  scalaVersion := "2.12.3",
  crossScalaVersions := Seq("2.11.8", "2.12.3"),
  addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3"),
  libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "3.0.0",
    "org.scalacheck" %% "scalacheck" % "1.13.4",
    "com.lihaoyi" %% "sourcecode" % "0.1.3"),
  scalacOptions ++= Seq(
    "-unchecked",
    "-deprecation",
    "-Ypartial-unification",
    "-Ywarn-unused-import",
    // "-Xprint:typer",
    // "-Xlog-implicit-conversions",
    "-feature",
    "-language:implicitConversions",
    "-language:postfixOps",
    "-language:higherKinds"),
  scalacOptions in (Compile, console) ~= (_ filterNot (_ == "-Ywarn-unused-import")),
  scalacOptions in (Test, console) := (scalacOptions in (Compile, console)).value,
  // Sonatype publishing conf.
  publishTo := sonatypePublishTo.value,
  sonatypeProfileName := "org.hablapps",
  publishMavenStyle := true,
  licenses := Seq("APL2" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
  sonatypeProjectHosting := Some(GitHubHosting("hablapps", "puretest", "juanmanuel.serrano@hablapps.com")),
  homepage := Some(url("https://github.com/hablapps/puretest")),
  scmInfo := Some(
    ScmInfo(
      url("https://github.com/hablapps/puretest"),
      "scm:git@github.com:hablapps/puretest.git"
    )
  ),
  developers := List(
    Developer(id="jserranohidalgo",
      name="Juan Manuel Serrano Hidalgo",
      email="juanmanuel.serrano@hablapps.com",
      url=url("http://www.hablapps.com")),
    Developer(id="javierfs89",
      name="Javier Fuentes Sánchez",
      email="javier.fuentes@hablapps.com",
      url=url("http://www.hablapps.com")),
    Developer(id="jeslg",
      name="Jesús López González",
      email="jesus.lopez@hablapps.com",
      url=url("http://www.hablapps.com"))
  )
)


lazy val root = (project in file("."))
  .aggregate(cats, scalaz, tictactoe)
  .settings(
    commonSettings,
    publishArtifact := false
  )

lazy val cats = project
  .settings(
    commonSettings,
    name := "puretest-cats",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "1.0.0-RC1",
      "org.typelevel" %% "cats-mtl-core" % "0.1.0" % "test"
    ))

lazy val scalaz = project
  .settings(
    commonSettings,
    name := "puretest-scalaz",
    libraryDependencies ++= Seq(
      "org.scalaz" %% "scalaz-core" % "7.2.9"
    ))

lazy val tictactoe = (project in file("examples/tictactoe"))
  .dependsOn(cats)
  .settings(
    commonSettings,
    name := "tictactoe-example",
    publishArtifact := false,
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-mtl-core" % "0.1.0" % "test",
      "org.http4s" %% "http4s-dsl" % "0.18.0-M5",
      "org.http4s" %% "http4s-blaze-server" % "0.18.0-M5",
      "org.http4s" %% "http4s-blaze-client" % "0.18.0-M5",
      "org.http4s" %% "http4s-circe" % "0.18.0-M5",
      "io.circe" %% "circe-generic" % "0.9.0-M1",
      "io.circe" %% "circe-literal" % "0.9.0-M1"))


