case class Entry(
  signalPatterns: List[String], // 10
  outputValues: List[String],   //  4
)

object Entry:
  def fromLine(line: String) =
    val lineParts = line.split(" [|] ")
    val left = lineParts(0).trim
    val right = lineParts(1).trim
    Entry(
      left .split(" ").toList,
      right.split(" ").toList,
    )

val nrSegments = Map(
  0 -> 6,
  1 -> 2,
  2 -> 5,
  3 -> 5,
  4 -> 4,
  5 -> 5,
  6 -> 6,
  7 -> 3,
  8 -> 7,
  9 -> 6,
)

def part1(entries: List[Entry]) =
  entries.flatMap(_.outputValues)
    .map(_.length)
    .count(v => 
      v == nrSegments(1) ||
      v == nrSegments(4) ||
      v == nrSegments(7) ||
      v == nrSegments(8)
    )

def part2(input: Any) =
  ()

@main
def run =
  val src = io.Source.fromFile("08.txt")
  val input: List[Entry] =
    try src.getLines
      .filter(_.nonEmpty)
      .map(Entry.fromLine)
      .toList
    finally src.close()

  println(part1(input))
  println(part2(input))

