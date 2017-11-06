scalaVersion := "2.12.4"

scalacOptions ++= Seq(
  "-deprecation", "-unchecked", "-feature", "-Xfuture", "-Xfatal-warnings"
)

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.0" % "test"
, "org.parboiled" %% "parboiled" % "2.1.3"
)
