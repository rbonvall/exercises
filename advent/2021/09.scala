import scala.util.chaining.scalaUtilChainingOps

type HeightMap = Map[(Int, Int), Int]

def matrixToHeightMap(matrix: List[List[Int]]): HeightMap =
  val asPairs =
    for
      (row, i) <- matrix.zipWithIndex
      (value, j) <- row.zipWithIndex
    yield (i, j) -> value
  asPairs.toMap

def findLowPoints(heightMap: HeightMap): Set[(Int, Int)] =
  val heightMapWithWall = heightMap.withDefaultValue(Int.MaxValue)
  heightMapWithWall
    .filter { case ((i, j), height) =>
      heightMapWithWall(i + 1, j    ) > height &&
      heightMapWithWall(i - 1, j    ) > height &&
      heightMapWithWall(i    , j + 1) > height &&
      heightMapWithWall(i    , j - 1) > height
    }
    .keySet

def findBasin(heightMap: HeightMap, lowPoint: (Int, Int)): Set[(Int, Int)] =
  val hm = heightMap.withDefaultValue(Int.MaxValue)
  def iter(p: (Int, Int), acc: Set[(Int, Int)]): Set[(Int, Int)] =
    if      acc contains p then acc
    else if hm(p) >= 9     then acc
    else
      val (i, j) = p
      val up    = (i - 1, j    )
      val down  = (i + 1, j    )
      val left  = (i    , j - 1)
      val right = (i    , j + 1)
      iter(up, iter(down, iter(left, iter(right, acc + p))))

  iter(lowPoint, Set.empty)


def part1(input: HeightMap) =
  findLowPoints(input)
    .to(Seq)
    .map((i, j) => input(i, j) + 1)
    .sum

def part2(input: HeightMap) =
  findLowPoints(input)
    .to(Seq)
    .map(p => findBasin(input, p))
    .map(_.size)
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

  val input = matrixToHeightMap(matrix)

  val example = matrixToHeightMap(List(
    List(2, 1, 9, 9, 9, 4, 3, 2, 1, 0),
    List(3, 9, 8, 7, 8, 9, 4, 9, 2, 1),
    List(9, 8, 5, 6, 7, 8, 9, 8, 9, 2),
    List(8, 7, 6, 7, 8, 9, 6, 7, 8, 9),
    List(9, 8, 9, 9, 9, 6, 5, 6, 7, 8),
  ))
  assert(part2(example) == 1134)

  println(part1(input))
  println(part2(input))

