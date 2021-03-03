import mill._
import scalalib._

object tapl extends ScalaModule {

  def scalaVersion = "2.12.4"
  def scalacOptions = Seq(
    "-deprecation",
    "-unchecked",
    "-feature",
    "-Xfuture",
    "-Xfatal-warnings"
  )

  object test extends Tests {
    def testFrameworks = Seq("org.scalatest.tools.Framework")
    def ivyDeps = Agg(
      ivy"org.scalatest::scalatest:3.0.0"
    )
  }

}

