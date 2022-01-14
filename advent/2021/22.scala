type Input = List[RebootStep]

case class ThreeDeeRange(
  xRange: Range,
  yRange: Range,
  zRange: Range,
):
  def all =
    for
      x <- xRange
      y <- yRange
      z <- zRange
    yield (x, y, z)

  def clipTo(m: Int): ThreeDeeRange =
    ThreeDeeRange(
      ((-m) max xRange.head) to (m min xRange.last),
      ((-m) max yRange.head) to (m min yRange.last),
      ((-m) max zRange.head) to (m min zRange.last),
    )

  def size =
    xRange.size * yRange.size * zRange.size


enum Status:
  case On, Off

case class RebootStep(
  status: Status,
  cubes: ThreeDeeRange,
)


object Input:
  val inputLine = """(on|off) x=(-?\d+)..(-?\d+),y=(-?\d+)..(-?\d+),z=(-?\d+)..(-?\d+)""".r

  def rebootStepFromLine(line: String): RebootStep =
    line match
      case inputLine(s, x0, x1, y0, y1, z0, z1) =>
        val status = s match
          case "on"  => Status.On
          case "off" => Status.Off
        RebootStep(
          status,
          ThreeDeeRange(
            x0.toInt to x1.toInt,
            y0.toInt to y1.toInt,
            z0.toInt to z1.toInt,
          )
        )

  def fromLines(lines: List[String]): Input =
    lines.map(rebootStepFromLine)

def runSteps(steps: List[RebootStep]): Set[(Int, Int, Int)] =
  val initial = Set.empty[(Int, Int, Int)]
  steps.foldLeft(initial) { (onCubes, step) =>
    val cubesToSwitch = step.cubes.all.toSet
    step.status match
      case Status.On  => onCubes union cubesToSwitch
      case Status.Off => onCubes diff  cubesToSwitch
  }

def part1(input: Input) =
  runSteps(
    input.map(step => step.copy(cubes = step.cubes.clipTo(50)))
  ).size

def part2(input: Input) =
  // runSteps(input).size  // <-- OutOfMemoryError
  ()

@main
def run =
  val src = io.Source.fromFile("22.txt")
  val input: Input =
    try Input.fromLines(src.getLines.filter(_.nonEmpty).toList)
    finally src.close()

  println(part1(input))
  println(part2(input))



