import Dependencies._

val commonSettings = Seq(
  scalaVersion := "2.12.1"
)

lazy val language = (project in file("language"))
  .settings(commonSettings)
  .settings(
    name := "Language",
    libraryDependencies += scalaTest % Test
  )
