import scala.io.Source
import java.io.File

extension (values: List[Int])
  def derive = (values.tail zip values.init).map(_ - _)

def part1(depths: List[Int]) =
  depths.derive
    .count(_ > 0)

def part2(depths: List[Int]) =
  depths.sliding(3)
    .map(_.sum)
    .toList
    .derive
    .count(_ > 0)

@main
def run =
  val src = Source.fromFile(File("01.txt"))
  val input: List[Int] =
    try src.getLines.map(_.toInt).toList
    finally src.close()

  println(part1(input))
  println(part2(input))
