//> using scala 3.7.4
//> using dep com.lihaoyi::pprint:0.9.6
//> using dep org.scalameta::munit:1.2.1

// $ scala-cli test 05.scala

case class Range(a: Long, b: Long):
    def contains(i: Long) = a <= i && i <= b
    def isLeftOf(that: Range) = b < that.a
    def isRightOf(that: Range) = that.b < a
    def size = b - a + 1

type Database = (freshIngredientIdRanges: List[Range], availableIngredientIds: List[Long])

def part1(db: Database) =
    db.availableIngredientIds.count: i =>
        db.freshIngredientIdRanges.exists(_.contains(i))

def part2(db: Database) =
    db.freshIngredientIdRanges
        .foldLeft(Nil)(insertRange)
        .map(_.size)
        .sum

val rangeRegex = """(\d+)-(\d+)""".r.anchored
val idRegex = """(\d+)""".r.anchored
def parseLines(lines: List[String]): Database =
    ( freshIngredientIdRanges = lines.collect { case rangeRegex(a, b) => Range(a.toLong, b.toLong) }
    , availableIngredientIds  = lines.collect { case idRegex(i) => i.toLong }
    )

// Inserts a range into a sequence of ascending, disjoint ranges,
// merging any ranges as needed to maintain this property.
def insertRange(ranges: List[Range], toInsert: Range): List[Range] =
    val merged = Range(
        a = ranges.collectFirst { case r if r.contains(toInsert.a) => r.a }.getOrElse(toInsert.a),
        b = ranges.collectFirst { case r if r.contains(toInsert.b) => r.b }.getOrElse(toInsert.b),
    )
    val (before, rest) = ranges.span(_.isLeftOf(merged))
    val after = rest.dropWhile(!_.isRightOf(merged))
    before ::: (merged :: after)


class Test05 extends munit.FunSuite:
    val example = parseLines(List(
        "3-5",
        "10-14",
        "16-20",
        "12-18",
        "",
        "1",
        "5",
        "8",
        "11",
        "17",
        "32",
    ))

    test("run"):
        val src = io.Source.fromFile("05.txt")
        val inputLines = try src.getLines.filter(_.nonEmpty).to(List) finally src.close()
        val db = parseLines(inputLines)
        pprint.log(part1(db))
        pprint.log(part2(db))

    test("part1"):
        assertEquals(part1(example), 3)

    test("part2"):
        assertEquals(part2(example), 14L)

    test("parseLines"):
        assertEquals(example.freshIngredientIdRanges, List(Range(3L, 5L), Range(10L, 14L), Range(16L, 20L), Range(12L, 18L)))
        assertEquals(example.availableIngredientIds,  List(1L, 5L, 8L, 11L, 17L, 32L))

    test("insertRange"):
        extension (a: Int) def ~ (b: Int) = Range(a.toLong, b.toLong)
        val ranges = List(10 ~ 20, 30 ~ 40, 50 ~ 60)
        assertEquals(insertRange(ranges,  3 ~  7), List(3 ~ 7, 10 ~ 20, 30 ~ 40, 50 ~ 60))
        assertEquals(insertRange(ranges,  3 ~ 17), List(3 ~         20, 30 ~ 40, 50 ~ 60))
        assertEquals(insertRange(ranges,  3 ~ 47), List(3 ~                  47, 50 ~ 60))
        assertEquals(insertRange(ranges,  3 ~ 67), List(3 ~                           67))

        assertEquals(insertRange(ranges, 10 ~ 17), ranges)
        assertEquals(insertRange(ranges, 13 ~ 19), ranges)
        assertEquals(insertRange(ranges, 15 ~ 20), ranges)

        assertEquals(insertRange(ranges, 15 ~ 17), List(10 ~ 20, 30 ~ 40, 50 ~ 60))
        assertEquals(insertRange(ranges, 15 ~ 28), List(10 ~ 28, 30 ~ 40, 50 ~ 60))
        assertEquals(insertRange(ranges, 15 ~ 48), List(10 ~          48, 50 ~ 60))
        assertEquals(insertRange(ranges, 15 ~ 61), List(10 ~                   61))

        assertEquals(insertRange(ranges, 22 ~ 28), List(10 ~ 20, 22 ~ 28, 30 ~ 40, 50 ~ 60))

        assertEquals(insertRange(ranges, 30 ~ 37), ranges)
        assertEquals(insertRange(ranges, 33 ~ 39), ranges)
        assertEquals(insertRange(ranges, 35 ~ 40), ranges)

        assertEquals(insertRange(ranges, 50 ~ 57), ranges)
        assertEquals(insertRange(ranges, 53 ~ 59), ranges)
        assertEquals(insertRange(ranges, 55 ~ 60), ranges)

        assertEquals(insertRange(ranges, 49 ~ 65), List(10 ~ 20, 30 ~ 40, 49 ~ 65))
        assertEquals(insertRange(ranges, 55 ~ 65), List(10 ~ 20, 30 ~ 40, 50 ~ 65))
        assertEquals(insertRange(ranges, 62 ~ 65), List(10 ~ 20, 30 ~ 40, 50 ~ 60, 62 ~ 65))
