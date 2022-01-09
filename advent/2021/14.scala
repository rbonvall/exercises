case class Input(
  template: String,
  rules: Map[(Char, Char), Char],
)
object Input:
  val ruleLine = """(\w+) -> (\w+)""".r
  def fromLines(lines: List[String]): Input =
    Input(
      lines.head,
      lines.collect { case ruleLine(pair, ins) => ((pair(0), pair(1)), ins(0)) }.toMap
    )

def insert(rules: Map[(Char, Char), Char])(polymer: String): String =
  val toInsert = (polymer.init zip polymer.tail).map(rules)
  val elements = (polymer zip toInsert).flatMap((p, i) => List(p, i)) :+ polymer.last
  elements.mkString

def part1(input: Input) =
  val states = LazyList.iterate(input.template)(insert(input.rules))
  val counts = states(10).groupBy(identity).map((k, v) => v.length)
  counts.max - counts.min

def part2(input: Input) =
  ()

@main
def run =
  val src = io.Source.fromFile("14.txt")
  val input: Input =
    try Input.fromLines(src.getLines.toList)
    finally src.close()

  assert(insert(Map(
    ('A', 'A') -> 'W',
    ('A', 'B') -> 'X',
    ('B', 'A') -> 'Y',
    ('B', 'B') -> 'Z',
  ))("ABBAB") == "AXBZBYAXB")

  println(part1(input))
  println(part2(input))


