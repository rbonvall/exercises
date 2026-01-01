//> using scala 3.7.4
//> using dep com.lihaoyi::pprint:0.9.6
//> using dep org.scalameta::munit:1.2.1

// $ scala-cli test 09.scala

case class Point2D(x: Long, y: Long)

def part1(redTiles: List[Point2D]) =
    0L

def part2(redTiles: List[Point2D]) =
    0L

def parseLine(line: String) =
    line.split(",").toList.map(_.toLong) match
        case List(x, y) => Point2D(x, y)
        case _ => ???

class Test09 extends munit.FunSuite:
    val example = List(
        "7,1",
        "11,1",
        "11,7",
        "9,7",
        "9,5",
        "2,5",
        "2,3",
        "7,3",
    ).map(parseLine)

    test("run"):
        val src = io.Source.fromFile("09.txt")
        val inputLines = try src.getLines.filter(_.nonEmpty).to(List) finally src.close()
        val redTiles = inputLines.map(parseLine)
        pprint.log(redTiles.length)
        pprint.log(part1(redTiles))
        pprint.log(part2(redTiles))

    test("part1"):
        assertEquals(part1(example), 50L)

    test("part2"):
        assertEquals(part2(example), 0L)

    test("parseLine"):
        assertEquals(example.head, Point2D(7, 1))

