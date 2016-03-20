name := "AMI_PP_MATRIX"

version := "1.0"

scalaVersion := "2.11.8"

val scalazVersion = "7.2.0"
val scalatestVersion = "2.2.6"

libraryDependencies += "org.scalaz" %% "scalaz-core" % scalazVersion
libraryDependencies += "org.scalaz" %% "scalaz-effect" % scalazVersion
libraryDependencies += "org.scalatest" %% "scalatest" % scalatestVersion % "test"
