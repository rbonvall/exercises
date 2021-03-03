import mill._
import scalalib._

object tapl extends ScalaModule {

  def scalaVersion = "3.0.0-RC1"
  def scalacOptions = Seq(
    "-deprecation",
    "-unchecked",
    "-feature",
    "-Xfatal-warnings"
  )

  object test extends Tests {
    def testFrameworks = Seq("org.scalatest.tools.Framework")
    def ivyDeps = Agg(
      ivy"org.scalatest::scalatest:3.0.0"
    )
  }

}

