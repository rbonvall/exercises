//> using scala 3.7.4
//> using dep com.lihaoyi::pprint:0.9.6
//> using dep org.scalameta::munit:1.2.2

// $ scala-cli test 02.scala

def part1(reports: List[List[Int]]) =
    reports.count(isSafe)

def part2(reports: List[List[Int]]) =
    reports.count(isSafeWithTolerance)

def isSafe(report: List[Int]) =
    val diffs = report.sliding(2).map(pair => pair(0) - pair(1)).toList
    diffs.forall(d => -3 <= d && d <= -1) || diffs.forall(d => 1 <= d && d <= 3)

def isSafeWithTolerance(report: List[Int]) =
    isSafe(report) || report.indices
        .map(report.patch(_, Nil, 1))
        .exists(isSafe)

class Test02 extends munit.FunSuite:
    val example = List(
        List(7, 6, 4, 2, 1),
        List(1, 2, 7, 8, 9),
        List(9, 7, 6, 2, 1),
        List(1, 3, 2, 4, 5),
        List(8, 6, 4, 4, 1),
        List(1, 3, 6, 7, 9),
    )

    test("run"):
        val src = io.Source.fromFile("02.txt")
        val reports =
            try src.getLines
                .filterNot(_.isEmpty)
                .map(_.split("[ ]").map(_.toInt).to(List))
                .toList
            finally src.close()

        pprint.log(part1(reports))
        pprint.log(part2(reports))

    test("part1"):
        assertEquals(part1(example), 2)

    test("part2"):
        assertEquals(part2(example), 4)

