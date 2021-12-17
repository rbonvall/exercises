trait VisitLog:
  def visit(s: String): VisitLog
  def canVisit(s: String): Boolean

class CanVisitSmallCavesOnlyOnce(visited: Set[String]) extends VisitLog:
  def visit(s: String) = CanVisitSmallCavesOnlyOnce(
    if s.charAt(0).isLower
    then visited + s
    else visited
  )
  def canVisit(s: String) = !visited.contains(s)

class CanVisitOneSmallCaveTwice(visited: Set[String], visitedTwice: Option[String]) extends VisitLog:
  def visit(s: String) =
    val isSmall = s.length == 1 && s.charAt(0).isLower
    if      !isSmall                                     then this
    else if !visited.contains(s)                         then CanVisitOneSmallCaveTwice(visited + s, visitedTwice)
    else if  visited.contains(s) && visitedTwice.isEmpty then CanVisitOneSmallCaveTwice(visited,     Some(s))
    else this
  def canVisit(s: String) = s != "start" && (!visited.contains(s) || visitedTwice.isEmpty)


def traversePaths(connections: List[(String, String)], visitLog: VisitLog): List[List[String]] =
  def iter(current: String, log: VisitLog): List[List[String]] =
    if current == "end" then List(List(current))
    else
      connections
        .collect {
          case (`current`, dst) if log.canVisit(dst) => dst
          case (dst, `current`) if log.canVisit(dst) => dst
        }
        .flatMap(dst => iter(dst, log.visit(current)))
        .map(path => current :: path)

  import util.chaining.scalaUtilChainingOps
  iter("start", visitLog)

def part1(connections: List[(String, String)]) =
  val emptyLog = CanVisitSmallCavesOnlyOnce(Set.empty)
  traversePaths(connections, emptyLog).length

def part2(connections: List[(String, String)]) =
  val emptyLog = CanVisitOneSmallCaveTwice(Set.empty, None)
  val paths = traversePaths(connections, emptyLog)
  paths.length

@main
def run =
  val src = io.Source.fromFile("12.txt")
  val input: List[(String, String)] =
    try src.getLines
      .filter(_.nonEmpty)
      .map { line =>
        val i = line.indexOf('-')
        (line.take(i), line.drop(i + 1))
      }
      .toList
    finally src.close()

  val example = List(
    ("start", "A"),
    ("start", "b"),
    ("A", "c"),
    ("A", "b"),
    ("b", "d"),
    ("A", "end"),
    ("b", "end"),
  )
  assert(part1(example) == 10)
  assert(part2(example) == 36)

  println(part1(input))
  println(part2(input))

