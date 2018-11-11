scalaVersion := "2.12.7"

scalacOptions ++= Seq(
  "-language:_",
  "-Ypartial-unification",
  "-Xfatal-warnings"
)

resolvers += Resolver.sonatypeRepo("releases")

libraryDependencies ++= Seq(
  "org.scalatest"        %% "scalatest"      % "3.0.5"  % "test"
, "org.scalacheck"       %% "scalacheck"     % "1.14.0" % "test"
, "com.github.mpilquist" %% "simulacrum"     % "0.13.0"
, "org.scalaz"           %% "scalaz-core"    % "7.2.26"
, "eu.timepit"           %% "refined"        % "0.9.3"
, "eu.timepit"           %% "refined-scalaz" % "0.9.3"
)

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.7")
addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full)

