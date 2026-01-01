//> using scala 3.7.4
//> using dep com.lihaoyi::pprint:0.9.6
//> using dep org.scalameta::munit:1.2.1

// $ scala-cli test 10.scala

case class Machine(diagram: String, buttons: List[List[Int]], joltages: List[Int])

def part1(ms: List[Machine]) =
    0L

def part2(ms: List[Machine]) =
    0L

def parseLine(line: String): Machine =
    val words = line.trim.split("[ ]+").to(Seq)
    Machine(
        diagram = words.head.replaceAll("[^.#]", ""),
        buttons = words.tail.init.to(List).map: word =>
            word.replaceAll("[()]", "")
                .split(",")
                .map(_.toInt)
                .to(List),
        joltages = words.last
            .replaceAll("[{}]", "")
            .split(",")
            .map(_.toInt)
            .to(List),
    )

class Test10 extends munit.FunSuite:
    val example = List(
        "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}",
        "[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}",
        "[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}",
    ).map(parseLine)

    test("run"):
        val src = io.Source.fromFile("10.txt")
        val inputLines = try src.getLines.filter(_.nonEmpty).to(List) finally src.close()
        val machines = inputLines.map(parseLine)
        pprint.log(example)
        pprint.log(machines.take(3))
        pprint.log(part1(machines))
        pprint.log(part2(machines))

    test("part1"):
        assertEquals(part1(example), 50L)

    test("part2"):
        assertEquals(part2(example), 0L)

    test("parseLine"):
        assertEquals(example.head, Machine(
            diagram = ".##.",
            buttons = List(List(3), List(1, 3), List(2), List(2, 3), List(0, 2), List(0, 1)),
            joltages = List(3, 5, 4, 7),
        ))

