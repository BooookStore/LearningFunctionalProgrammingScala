import Dependencies._

val commonSettings = Seq(
  scalaVersion := "2.12.1",
  libraryDependencies += scalaTest % Test
)

lazy val language = (project in file("language"))
  .settings(commonSettings)
  .settings(
    name := "Language",
  )

lazy val errorhandling = (project in file("errorhandling"))
  .settings(commonSettings)
  .settings(
    name := "errorhandling"
  )

lazy val datastructure = (project in file("datastructure"))
  .settings(commonSettings)
  .settings(
    name := "datastructure"
  )

lazy val nonstrictfunction = (project in file("nonStrictFunction"))
  .settings(commonSettings)
  .settings(
    name := "nonstrictfunction"
  )
