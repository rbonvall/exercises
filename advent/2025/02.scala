//> using scala 3.7.4
//> using dep com.lihaoyi::pprint:0.9.6
//> using dep org.scalameta::munit:1.2.1

// $ scala-cli test 02.scala

case class Range(first: Long, last: Long):
    require(first <= last)
    def sum: Long = (last + first) * (last - first + 1) / 2
    def nrDigits: Int = first.nrDigits
    def contains(n: Long) = first <= n && n <= last

    // Splits a range into smaller ranges for which all their values have the same length.
    def splitByNrDigits: Seq[Range] =
        val da = first.nrDigits
        val db = last.nrDigits
        if da == db then Seq(this)
        else
            val intermediateRanges =
                for d <- (da + 1) to (db - 1)
                yield Range(pow10(d - 1), pow10(d) - 1)  // 10...0 to 99...9
            Range(first, pow10(da) - 1) +: intermediateRanges :+ Range(pow10(db - 1), last)

    def toRepeating(nrTimes: Int): Option[Range] =
        for length <- Option.when(nrDigits % nrTimes == 0)(nrDigits / nrTimes)
            a = nextRepeatingPart(first, length)
            b = prevRepeatingPart(last,  length)
            if a <= b
        yield Range(a, b)

extension (n: Long)
    def nrDigits: Int = (math.floor(math.log10(n)) + 1).toInt
    def repeat(times: Int): Long = (n.toString * times).toLong

// Precompute all long powers of 10
val pow10: Seq[Long] = Iterator.iterate(1L)(_ * 10L)
    .take(19)
    .to(collection.immutable.ArraySeq)

val idRangeRegex = "([0-9]+)-([0-9]+)".r
def parseInput(input: String): Seq[Range] =
    input.replaceAll("\\s", "")
        .split(",")
        .to(Vector)
        .collect { case idRangeRegex(a, b) => Range(a.toLong, b.toLong) }

def part1(ranges: Seq[Range]) =
   ranges.flatMap(_.toRepeating(2))
       .map(r => (pow10(r.nrDigits) + 1L) * r.sum)
       .sum

def part2(ranges: Seq[Range]) =
    0L

def nextRepeatingPart(num: Long, length: Int): Long =
    require(num.nrDigits % length == 0)
    val parts = num.toString.grouped(length).map(_.toLong).toList
    if parts.find(_ != parts.head).exists(_ >= parts.head)
    then parts.head + 1
    else parts.head

def prevRepeatingPart(num: Long, length: Int): Long =
    require(num.nrDigits % length == 0)
    val parts = num.toString.grouped(length).map(_.toLong).toList
    if parts.find(_ != parts.head).exists(_ <= parts.head)
    then parts.head - 1
    else parts.head

class Test02 extends munit.FunSuite:
    val example = """
        11-22,95-115,998-1012,1188511880-1188511890,222220-222224,
        1698522-1698528,446443-446449,38593856-38593862,565653-565659,
        824824821-824824827,2121212118-2121212124
    """
    val exampleRanges = parseInput(example).flatMap(_.splitByNrDigits)

    test("run"):
        val src = io.Source.fromFile("02.txt")
        val inputRanges = try parseInput(src.mkString) finally src.close()

        // All functions assume a range contain values having the same number of digits,
        // thus we start by splitting the input ranges to ensure this property holds.
        val splitRanges = inputRanges.flatMap(_.splitByNrDigits)
        pprint.log(part1(splitRanges))
        pprint.log(part2(splitRanges))

    test("part1"):
        assertEquals(part1(exampleRanges), 1227775554L)
    test("part2"):
        assertEquals(part2(exampleRanges), 0L)
    test("Long#repeat"):
        assertEquals(678L.repeat(3), 678678678L)
    test("Range#splitByNrDigits"):
        assertEquals( Range(45,  345).splitByNrDigits, Seq(Range(45, 99), Range(100, 345)) )
        assertEquals( Range(45, 1345).splitByNrDigits, Seq(Range(45, 99), Range(100, 999), Range(1000, 1345)) )
        assertEquals( Range(456, 789).splitByNrDigits, Seq(Range(456, 789)) )
        // Idempotency:
        assertEquals(exampleRanges, exampleRanges.flatMap(_.splitByNrDigits))
    test("Range#toRepeating"):
        assertEquals( Range(381951, 414407).toRepeating(3), Some(Range( 38,  41)) ) // range contains 383838...414141
        assertEquals( Range(381951, 414407).toRepeating(2), Some(Range(382, 413)) ) // range contains 382382...413413
        assertEquals( Range(381381, 419419).toRepeating(2), Some(Range(381, 419)) ) // range contains 381381...419419
        assertEquals( Range(  2987,   3020).toRepeating(2), None                  ) // range doesn't contain any values like ABAB
    test("nextRepeatingPart"):
        assertEquals( nextRepeatingPart(341213566L, 3), 341L )
        assertEquals( nextRepeatingPart(341341299L, 3), 341L )
        assertEquals( nextRepeatingPart(341341341L, 3), 341L )
        assertEquals( nextRepeatingPart(341513566L, 3), 342L )
        assertEquals( nextRepeatingPart(214, 1), 2L )
        assertEquals( nextRepeatingPart(222, 1), 2L )
        assertEquals( nextRepeatingPart(234, 1), 3L )
    test("prevRepeatingPart"):
        assertEquals( prevRepeatingPart(341213566L, 3), 340L )
        assertEquals( prevRepeatingPart(341341299L, 3), 340L )
        assertEquals( prevRepeatingPart(341341341L, 3), 341L )
        assertEquals( prevRepeatingPart(341513566L, 3), 341L )
        assertEquals( prevRepeatingPart(214, 1), 1L )
        assertEquals( prevRepeatingPart(222, 1), 2L )
        assertEquals( prevRepeatingPart(234, 1), 2L )

