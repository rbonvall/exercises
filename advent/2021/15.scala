import scala.collection.immutable.ArraySeq

case class Input(matrix: ArraySeq[ArraySeq[Int]]):
  require(matrix.forall(_.length == matrix.head.length))
  val height = matrix.length
  val width = matrix.head.length

  def get(i: Int, j: Int) =
    matrix(i)(j)

object Input:
  def fromLines(lines: List[String]): Input =
    Input(lines.to(ArraySeq).map(_.map(_.toString.toInt).to(ArraySeq)))


def part1(input: Input) =
  ()

def part2(input: Input) =
  ()


@main
def run =
  val src = io.Source.fromFile("15.txt")
  val input: Input =
    try Input.fromLines(src.getLines.filter(_.nonEmpty).toList)
    finally src.close()

  assert(input.width  == 100)
  assert(input.height == 100)
  assert(input.get( 0,  0) == 8)
  assert(input.get(99, 99) == 9)

  println(part1(input))
  println(part2(input))


