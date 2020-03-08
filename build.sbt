name := "poker"

version := "0.1"

scalaVersion := "2.13.1"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "2.1.1",
  "org.typelevel" %% "cats-effect" % "2.1.1",
  "com.beachape" %% "enumeratum" % "1.5.15",
  "org.scalatest" %% "scalatest" % "3.1.0" % Test
)
