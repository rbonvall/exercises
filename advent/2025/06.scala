//> using scala 3.7.4
//> using dep com.lihaoyi::pprint:0.9.6
//> using dep org.scalameta::munit:1.2.1

// $ scala-cli test 06.scala

case class Problem(numbers: List[String], op: Char):
    def applyOp(x: Long, y: Long) = op match
        case '+' => x + y
        case '*' => x * y
    def cephalopodNumbers =
        numbers.map(_.toList)
            .transpose
            .reverse
            .map(_.filterNot(_.isSpaceChar).mkString.toLong)
    def result1 = numbers.map(_.trim.toLong).reduce(applyOp)
    def result2 = cephalopodNumbers.reduce(applyOp)

def parseLines(lines: List[String]): List[Problem] =
    val startColumns: List[Int] = lines.last
        .zipWithIndex
        .collect { case ('+' | '*', j) => j }
        .to(List)
    val endColumns = startColumns.tail.map(_ - 1) :+ lines.head.length
    lines
        .map: line =>
            startColumns.zip(endColumns).map(line.slice)
        .transpose
        .map: tokens =>
            Problem(
                numbers = tokens.init,
                op = tokens.last.charAt(0)
            )

def part1(ps: List[Problem]) =
    ps.map(_.result1).sum

def part2(ps: List[Problem]) =
    ps.map(_.result2).sum

class Test06 extends munit.FunSuite:
    val example = parseLines(List(
        "123 328  51 64 ",
        " 45 64  387 23 ",
        "  6 98  215 314",
        "*   +   *   +  ",
    ))

    test("run"):
        val src = io.Source.fromFile("06.txt")
        val inputLines = try src.getLines.filter(_.nonEmpty).to(List) finally src.close()
        val problems = parseLines(inputLines)
        pprint.log(problems.takeRight(10))
        pprint.log(part1(problems))
        pprint.log(part2(problems))

    test("part1"):
        assertEquals(part1(example), 4277556L)

    test("part2"):
        assertEquals(part2(example), 3263827L)

    test("parseLines"):
        assertEquals(example, List(
            Problem(List("123", " 45", "  6"), '*'),
            Problem(List("328", "64 ", "98 "), '+'),
            Problem(List(" 51", "387", "215"), '*'),
            Problem(List("64 ", "23 ", "314"), '+'),
        ))

    test("Problem#cephalopodNumbers"):
        assertEquals(example(0).cephalopodNumbers, List[Long](356, 24, 1))
        assertEquals(example(1).cephalopodNumbers, List[Long](8, 248, 369))

