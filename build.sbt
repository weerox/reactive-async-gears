lazy val root = project
  .in(file("."))
  .settings(
    name := "reactive-async-gears",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := "3.4.0",
    libraryDependencies += "ch.epfl.lamp" %% "gears" % "0.2.0",
    libraryDependencies += "org.scala-lang.modules" %% "scala-collection-contrib" % "0.3.0",
    libraryDependencies += "org.scalameta" %% "munit" % "1.0.0" % Test
  )
