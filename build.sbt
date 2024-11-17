lazy val Benchmark = config("bench") extend Runtime

lazy val root = project
  .in(file("."))
  .settings(
    name         := "reactive-async-gears",
    version      := "0.1.0-SNAPSHOT",
    scalaVersion := "3.5.2",
    scalacOptions += "-explain",
    scalacOptions += "-Wunused:all",
    libraryDependencies += "ch.epfl.lamp" %% "gears" % "0.2.0",
    libraryDependencies += "org.scala-lang.modules" %% "scala-collection-contrib" % "0.3.0",
    libraryDependencies += "org.scalameta" %% "munit" % "1.0.0" % Test,
    libraryDependencies += ("com.storm-enroute" %% "scalameter" % "0.21" % Benchmark)
      .cross(CrossVersion.for3Use2_13),
    semanticdbEnabled := true,
    Compile / doc / scalacOptions ++= Seq("-private"),
    Compile / doc / scalacOptions ++= Seq(
      "-external-mappings:.*gears.*::scaladoc3::https://lampepfl.github.io/gears/api/"
    )
  )
  .configs(Benchmark)
  .settings(
    inConfig(Benchmark)(Defaults.testSettings),
    inConfig(Benchmark)(org.scalafmt.sbt.ScalafmtPlugin.scalafmtConfigSettings),
    scalafixConfigSettings(Benchmark),
    Benchmark / testFrameworks += new TestFramework(
      "org.scalameter.ScalaMeterFramework"
    ),
    Benchmark / parallelExecution := false,
    Benchmark / fork              := true,
    Benchmark / logBuffered       := false,
    Benchmark / javaOptions ++= Seq(
      "-Xmx24G",
      "-Xms1G"
    )
  )
