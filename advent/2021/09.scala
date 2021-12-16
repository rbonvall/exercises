def findLowPoints(heightMap: Map[(Int, Int), Int]): Set[(Int, Int)] =
  val heightMapWithWall = heightMap.withDefaultValue(Int.MaxValue)
  heightMapWithWall
    .filter { case ((i, j), height) =>
      heightMapWithWall(i + 1, j    ) > height &&
      heightMapWithWall(i - 1, j    ) > height &&
      heightMapWithWall(i    , j + 1) > height &&
      heightMapWithWall(i    , j - 1) > height
    }
    .keySet

def findBasin(heightMap: Map[(Int, Int), Int], lowPoint: (Int, Int)): Set[(Int, Int)] =
  // TODO: implement.
  Set(lowPoint)

def part1(input: Map[(Int, Int), Int]) =
  findLowPoints(input)
    .to(Seq)
    .map((i, j) => input(i, j) + 1)
    .sum

def part2(input: Map[(Int, Int), Int]) =
  findLowPoints(input)
    .to(Seq)
    .map(p => findBasin(input, p).size)
    .sortBy(n => -n)
    .take(3)
    .product

@main
def run =
  val src = io.Source.fromFile("09.txt")
  val matrix: List[List[Int]] =
    try src.getLines
      .filter(_.nonEmpty)
      .map(_.map(_.toString.toInt).to(List))
      .to(List)
    finally src.close()

  val asPairs =
    for
      (row, i) <- matrix.zipWithIndex
      (value, j) <- row.zipWithIndex
    yield (i, j) -> value

  val input = asPairs.toMap

  println(part1(input))
  println(part2(input))

