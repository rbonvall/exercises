import scala.collection.immutable.ArraySeq

def part1(input: Map[(Int, Int), Int]) =
  val inputWithWall = input.withDefaultValue(Int.MaxValue)
  inputWithWall
    .filter { case ((i, j), height) =>
      inputWithWall(i + 1, j    ) > height &&
      inputWithWall(i - 1, j    ) > height &&
      inputWithWall(i    , j + 1) > height &&
      inputWithWall(i    , j - 1) > height
    }
    .map { case ((i, j), height) => height + 1 }
    .sum

def part2(x: Any) =
  ()

@main
def run =
  val src = io.Source.fromFile("09.txt")
  val matrix: ArraySeq[ArraySeq[Int]] =
    try src.getLines
      .filter(_.nonEmpty)
      .map(_.map(_.toString.toInt).to(ArraySeq))
      .to(ArraySeq)
    finally src.close()

  val asPairs =
    for
      (row, i) <- matrix.zipWithIndex
      (value, j) <- row.zipWithIndex
    yield (i, j) -> value

  val input = asPairs.toMap

  println(part1(input))
  println(part2(input))

