enum Axis:
  case X, Y

case class Input(
  dots: List[(Int, Int)],
  folds: List[(Axis, Int)],
)
object Input:
  val dotLine = """(\d+),(\d+)""".r
  val foldInstruction = """fold along (x|y)=(\d+)""".r
  def fromLines(lines: List[String]): Input =
    Input(
      dots = lines.collect {
        case dotLine(x, y) => (x.toInt, y.toInt)
      },
      folds = lines.collect {
        case foldInstruction("x", x) => (Axis.X, x.toInt)
        case foldInstruction("y", y) => (Axis.Y, y.toInt)
      },
    )

def foldDot(axis: Axis, pos: Int)(dot: (Int, Int)): (Int, Int) =
  val (x, y) = dot
  if      axis == Axis.X && x > pos then (2 * pos - x, y)
  else if axis == Axis.Y && y > pos then (x, 2 * pos - y)
  else                                   dot

def part1(input: Input) =
  val (axis, pos) = input.folds.head
  input.dots
    .map(foldDot(axis, pos))
    .distinct
    .length

def part2(input: Input) =
  val finalDots: Set[(Int, Int)] =
    input.folds
      .foldLeft(input.dots) { case (dots, (axis, pos)) =>
        dots.map(foldDot(axis, pos)).distinct
      }
      .toSet
  val xMax = finalDots.map(_._1).max
  val yMax = finalDots.map(_._2).max
  val matrix =
    for y <- 0 to yMax yield
      for x <- 0 to xMax yield
        if finalDots contains (x, y)
        then "#"
        else " "
  matrix.map(_.mkString).mkString("\n")

@main
def run =
  val src = io.Source.fromFile("13.txt")
  val input: Input =
    try Input.fromLines(src.getLines.toList)
    finally src.close()

  assert(input.dots.head == (994, 18))
  assert(input.dots.last == (339, 28))
  assert(input.folds.head == (Axis.X, 655))
  assert(input.folds.last == (Axis.Y, 6))

  println(part1(input))
  println(part2(input))

