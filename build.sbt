name := "scala-starter"
version := "1.0.0"

lazy val starter_2_12 = project.in(file("starter-2.12")).settings(scalaVersion := "2.12.13")
lazy val starter_2_13 = project.in(file("starter-2.13")).settings(scalaVersion := "2.13.5")