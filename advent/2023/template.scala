//> using scala 3.5
//> using dep org.scalameta::munit:1.0.2
//> using option -deprecation

@main
def run =
    val src = io.Source.fromFile("XX.txt")
    val lines =
        try src.getLines
            .filterNot(_.isEmpty)
            .to(Array)
        finally src.close()

    println(part1(lines))
    println(part2(lines))

def part1(input: Any) =
    1

def part2(input: Any) =
    2


class TestsXX extends munit.FunSuite:

    test("example"):
        assertEquals(2 + 2, 4)
