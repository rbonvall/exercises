import scalaz._
import Scalaz._
import simulacrum._

object HelloWorld {
  def main(args: Array[String]): Unit = {

    val a = 1.right[String]
    val b = 2.right[String]
    val c = "sorry, no c".left[Int]

    val r1 =
      for {
        i <- a
        j <- b
        k <- c
      } yield i + j + k

    val r2 =
      for {
        i <- a
        j <- b
        k <- b
      } yield i + j + k

    println(r1)
    println(r2)
  }
}

