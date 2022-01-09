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

def part1(input: Input) =
  ()

def part2(input: Input) =
  ()

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

