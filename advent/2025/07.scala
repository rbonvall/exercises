//> using scala 3.7.4
//> using dep com.lihaoyi::pprint:0.9.6
//> using dep org.scalameta::munit:1.2.1

// $ scala-cli test 07.scala

def part1(ps: List[String]) =
    0

def part2(ps: List[String]) =
    0

class Test07 extends munit.FunSuite:
    val example = List(
        ".......S.......",
        "...............",
        ".......^.......",
        "...............",
        "......^.^......",
        "...............",
        ".....^.^.^.....",
        "...............",
        "....^.^...^....",
        "...............",
        "...^.^...^.^...",
        "...............",
        "..^...^.....^..",
        "...............",
        ".^.^.^.^.^...^.",
        "...............",
    )

    test("run"):
        val src = io.Source.fromFile("07.txt")
        val lines = try src.getLines.filter(_.nonEmpty).to(List) finally src.close()
        pprint.log(part1(lines))
        pprint.log(part2(lines))

    test("part1"):
        assertEquals(part1(example), 21)

    test("part2"):
        assertEquals(part2(example), 0)

