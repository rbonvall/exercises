case class Point(x: Int, y: Int)

def ranging(a: Int, b: Int): Range =
  if   a <= b then a to b
  else             (b to a).reverse

case class Vent(from: Point, to: Point):
  val dx = to.x - from.x
  val dy = to.y - from.y
  val isDiagonal = dx != 0 && dy != 0
  def xs = ranging(from.x, to.x)
  def ys = ranging(from.y, to.y)
  def coveredPoints =
    if      dx == 0          then ys.map { Point(from.x, _) }
    else if dy == 0          then xs.map { Point(_, from.y) }
    else if dx.abs == dy.abs then xs.zip(ys).map { Point(_, _) }
    else                          IndexedSeq.empty

def countOverlappingPoints(vents: List[Vent]): Int =
  vents
    .flatMap(_.coveredPoints)
    .foldLeft(Map.empty[Point, Int]) { (counter, point) =>
      counter.updated(point, counter.getOrElse(point, 0) + 1)
    }
    .count { (k, v) => v >= 2 }

def part1(vents: List[Vent]) =
  countOverlappingPoints(vents.filter(!_.isDiagonal))

def part2(vents: List[Vent]) =
  countOverlappingPoints(vents)

@main
def run =
  val src = io.Source.fromFile("05.txt")
  val linePattern = """(\d+),(\d+) -> (\d+),(\d+)""".r
  val input: List[Vent] =
    try src.getLines
      .collect {
        case linePattern(x1, y1, x2, y2) =>
          Vent(Point(x1.toInt, y1.toInt), Point(x2.toInt, y2.toInt))
      }
      .toList
    finally src.close()

  assert(input.length == 500)

  assert(Vent(Point(1, 1), Point(1, 3)).coveredPoints == List(Point(1, 1), Point(1, 2), Point(1, 3)))
  assert(Vent(Point(9, 7), Point(7, 7)).coveredPoints == List(Point(9, 7), Point(8, 7), Point(7, 7)))
  assert(Vent(Point(1, 1), Point(3, 3)).coveredPoints == List(Point(1, 1), Point(2, 2), Point(3, 3)))
  assert(Vent(Point(9, 7), Point(7, 9)).coveredPoints == List(Point(9, 7), Point(8, 8), Point(7, 9)))

  println(part1(input))
  println(part2(input))

