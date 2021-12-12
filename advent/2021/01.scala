import scala.io.Source
import java.io.File


def problem1(depths: List[Int]) =
  (depths.tail zip depths.init)
    .map(_ - _)
    .count(_ > 0)

def problem2(depths: List[Int]) =
  ()

@main
def run =
  val src = Source.fromFile(File("01.txt"))
  val input: List[Int] =
    try src.getLines.map(_.toInt).toList
    finally src.close()

  println(problem1(input))
  println(problem2(input))
