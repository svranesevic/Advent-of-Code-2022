lazy val root = (project in file("."))
  .settings(
    name := "adventofcode_2022",
    scalaVersion := "2.13.9",
    libraryDependencies += "org.typelevel" %% "cats-parse" % "0.3.7"
  )
