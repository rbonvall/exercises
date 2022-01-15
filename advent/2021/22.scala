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

def ticksFromRange(r: Range): List[Int] =
  List(r.head, r.last + 1)

def emptyRegionMapFromThreeDeeRanges(threeDeeRanges: List[ThreeDeeRange]): CubeRegionMap =
  val xTicks = threeDeeRanges.flatMap(r => ticksFromRange(r.xRange)).to(Vector).distinct.sorted
  val yTicks = threeDeeRanges.flatMap(r => ticksFromRange(r.yRange)).to(Vector).distinct.sorted
  val zTicks = threeDeeRanges.flatMap(r => ticksFromRange(r.zRange)).to(Vector).distinct.sorted
  CubeRegionMap(xTicks, yTicks, zTicks)

def part1(input: Input) =
  runSteps(
    input.map(step => step.copy(cubes = step.cubes.clipTo(50)))
  ).size

def part2(input: Input) =
  val initial = emptyRegionMapFromThreeDeeRanges(input.map(_.cubes))
  println(s"${initial.xAxisTicks.length} x ${initial.yAxisTicks.length} x ${initial.zAxisTicks.length}")
  input.foldLeft(initial) { case (regionMap, r@RebootStep(status, ThreeDeeRange(xs, ys, zs))) =>
    println(r)
    regionMap.setRegion(status == Status.On, xs, ys, zs)
  }.totalSize

@main
def run =
  val src = io.Source.fromFile("22.txt")
  val input: Input =
    try Input.fromLines(src.getLines.filter(_.nonEmpty).toList)
    finally src.close()

  println(part1(input))
  println(part2(input))


// --- Implementation of a custom data structure for this problem ---

import scala.collection.immutable.BitSet

case class CubeRegionMap(
  xAxisTicks: Vector[Int],
  yAxisTicks: Vector[Int],
  zAxisTicks: Vector[Int],
  cubes: BitSet = BitSet.empty,
):

  val xAxisTickIndex: Map[Int, Int] = xAxisTicks.zipWithIndex.toMap
  val yAxisTickIndex: Map[Int, Int] = yAxisTicks.zipWithIndex.toMap
  val zAxisTickIndex: Map[Int, Int] = zAxisTicks.zipWithIndex.toMap

  val X = xAxisTicks.size
  val Y = yAxisTicks.size
  val Z = zAxisTicks.size

  def rank(i: Int, j: Int, k: Int): Int =
    i + j * X + k * X * Y

  def unrank(n: Int): (Int, Int, Int) =
    val i = n % X
    val j = (n / X) % Y
    val k = (n / (X * Y)) % Z
    (i, j, k)

  def setRegion(on: Boolean, xRange: Range, yRange: Range, zRange: Range): CubeRegionMap =
    val i0 = xAxisTickIndex(xRange.head)
    val i1 = xAxisTickIndex(xRange.last + 1)
    val j0 = yAxisTickIndex(yRange.head)
    val j1 = yAxisTickIndex(yRange.last + 1)
    val k0 = zAxisTickIndex(zRange.head)
    val k1 = zAxisTickIndex(zRange.last + 1)
    val regionRanks =
      for
        i <- i0 until i1
        j <- j0 until j1
        k <- k0 until k1
      yield rank(i, j, k)
    this.copy(
      cubes =
        if on
        then cubes union regionRanks.toSet
        else cubes diff  regionRanks.toSet
    )

  def cubeSize(i: Int, j: Int, k: Int): Int =
    val dx = xAxisTicks(i + 1) - xAxisTicks(i)
    val dy = yAxisTicks(j + 1) - yAxisTicks(j)
    val dz = zAxisTicks(k + 1) - zAxisTicks(k)
    dx * dy * dz

  def totalSize =
    cubes.iterator.map(unrank).map(cubeSize).sum

