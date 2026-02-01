//> using scala 3.7.4
//> using dep com.lihaoyi::pprint:0.9.6
//> using dep org.scalameta::munit:1.2.2

// $ scala-cli test 01.scala

def part1(list0: List[Int], list1: List[Int]) =
    list0.sorted
        .zip(list1.sorted)
        .map((a, b) => (a - b).abs)
        .sum

def part2(list0: List[Int], list1: List[Int]) =
    val counts = list1.groupBy(identity)
        .mapValues(_.length)
        .toMap
        .withDefaultValue(0)
    list0.map(a => a * counts(a)).sum

def readLines(lines: List[String]): (List[Int], List[Int]) =
    val lol: List[List[Int]] = lines.map(_.trim.split("[ ]+").toList.map(_.toInt))
        .transpose
    (lol(0), lol(1))

class Test01 extends munit.FunSuite:
    val exampleLines = List(
        "3   4",
        "4   3",
        "2   5",
        "1   3",
        "3   9",
        "3   3",
    )
    val (exampleList0, exampleList1) = readLines(exampleLines)

    test("run"):
        val src = io.Source.fromFile("01.txt")
        val (list0, list1) =
            try readLines(src.getLines.filterNot(_.isEmpty).to(List))
            finally src.close()

        pprint.log(part1(list0, list1))
        pprint.log(part2(list0, list1))

    test("readLines"):
        assertEquals(exampleList0, List(3, 4, 2, 1, 3, 3))
        assertEquals(exampleList1, List(4, 3, 5, 3, 9, 3))

    test("part1"):
        assertEquals(part1(exampleList0, exampleList1), 11)

    test("part2"):
        assertEquals(part2(exampleList0, exampleList1), 31)

