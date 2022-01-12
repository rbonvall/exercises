import scala.math.Numeric.Implicits.*

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

def pairs[T](ts: Seq[T]): Seq[(T, T)] =
  ts.init zip ts.tail

def count[T](ts: Seq[T]): Map[T, Long] =
  ts.groupBy(identity)
    .map((k, v) => (k, v.length.toLong))
    .withDefaultValue(0L)

def mergeCounts[T, N](counts1: Map[T, N], counts2: Map[T, N])
                     (using n: Numeric[N]): Map[T, N] =
  (counts1.keySet union counts2.keySet)
    .map(k => k -> (counts1.getOrElse(k, n.zero) + counts2.getOrElse(k, n.zero)))
    .toMap

def updatePairCountAfterInsertion(rules: Map[(Char, Char), Char])
                                 (counts: Map[(Char, Char), Long]): Map[(Char, Char), Long] =
  counts
    .map { case (pair @ (c1, c2), count) =>
      val i = rules(pair)
      Map(
        (c1, i) -> count,
        (i, c2) -> count,
      )
    }
    .reduce(mergeCounts)

def iteratePairCounts(input: Input): LazyList[Map[(Char, Char), Long]] =
  val initialCount = count(pairs(input.template))
  LazyList.iterate(initialCount)(updatePairCountAfterInsertion(input.rules))

def countPairEndings[T, N](pairCount: Map[(T, T), N])
                          (using n: Numeric[N]): Map[T, N] =
  pairCount.to(Seq)
    .groupBy { case ((c1, c2), n) => c2 }
    .map { (c2, counts) => (c2, counts.map(_._2).sum) }
    .toMap
    .withDefaultValue(n.zero)

def insert(rules: Map[(Char, Char), Char])(polymer: String): String =
  val toInsert = (polymer.init zip polymer.tail).map(rules)
  val elements = (polymer zip toInsert).flatMap((p, i) => List(p, i)) :+ polymer.last
  elements.mkString

def part1(input: Input) =
  val states = LazyList.iterate(input.template)(insert(input.rules))
  val counts = states(10).groupBy(identity).map((k, v) => v.length)
  counts.max - counts.min

def part2(input: Input) =
  val states = iteratePairCounts(input)
  val endingLetterCounts: Map[Char, Long] = countPairEndings(states(40))

  // After counting pair ending letters, we still need to account for the first letter from the input.
  val first = input.template.charAt(0)
  val letterCounts: Map[Char, Long] = endingLetterCounts.updated(first, endingLetterCounts(first) + 1)

  val counts = letterCounts.values
  counts.max - counts.min

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

  assert(mergeCounts(
    Map('a' -> 2, 'b' -> 3),
    Map('b' -> 4, 'c' -> 8)
  ) == Map('a' -> 2, 'b' -> 7, 'c' -> 8), "mergeCounts")

  val cs = count("abbcab".toList)
  assert(cs('a') == 2)
  assert(cs('b') == 3)
  assert(cs('c') == 1)
  assert(cs('d') == 0)

  val example = Input("NNCB", Map(
    ('C', 'H') -> 'B', ('H', 'H') -> 'N', ('C', 'B') -> 'H', ('N', 'H') -> 'C',
    ('H', 'B') -> 'C', ('H', 'C') -> 'B', ('H', 'N') -> 'C', ('N', 'N') -> 'C',
    ('B', 'H') -> 'H', ('N', 'C') -> 'B', ('N', 'B') -> 'B', ('B', 'N') -> 'B',
    ('B', 'B') -> 'N', ('B', 'C') -> 'B', ('C', 'C') -> 'N', ('C', 'N') -> 'C',
  ))
  assert(part1(example) == 1588, "!")
  assert(part2(example) == 2188189693529L, "!!")

  println(part1(input))
  println(part2(input))


