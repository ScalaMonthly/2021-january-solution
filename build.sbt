name := "2021-january"

version := "0.1"

scalaVersion := "2.13.5"

val compilerPlugins = List(
  compilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")
)

lazy val libraries = List(
  "org.typelevel" %% "cats-parse" % "0.3.2",
  "org.scalameta" %% "munit" % "0.7.23" % Test
)

lazy val parsersGambit = (project in file(".")).settings(
  libraryDependencies ++= libraries ++ compilerPlugins,
  testFrameworks += new TestFramework("munit.Framework")
)