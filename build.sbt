lazy val root = project
  .in(file("."))
  .settings(
    name := "reactive-async-gears",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := "3.4.0",
    libraryDependencies += "org.scalameta" %% "munit" % "1.0.0" % Test
  )
