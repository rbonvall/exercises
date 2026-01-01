//> using scala 3.7.4
//> using dep com.lihaoyi::pprint:0.9.6
//> using dep org.scalameta::munit:1.2.1

// $ scala-cli test 11.scala

case class Device(name: String, outputs: List[String])

def part1(ms: List[Device]) =
    0L

def part2(ms: List[Device]) =
    0L

def parseLine(line: String): Device =
    val words = line.trim.replaceFirst(":", "").split("[ ]+").to(List)
    Device(words.head, words.tail)


class Test11 extends munit.FunSuite:
    val example = List(
      "aaa: you hhh",
      "you: bbb ccc",
      "bbb: ddd eee",
      "ccc: ddd eee fff",
      "ddd: ggg",
      "eee: out",
      "fff: out",
      "ggg: out",
      "hhh: ccc fff iii",
      "iii: out",
    ).map(parseLine)

    test("run"):
        val src = io.Source.fromFile("11.txt")
        val inputLines = try src.getLines.filter(_.nonEmpty).to(List) finally src.close()
        val devices = inputLines.map(parseLine)
        pprint.log(example)
        pprint.log(devices.take(3))
        pprint.log(part1(devices))
        pprint.log(part2(devices))

    test("part1"):
        assertEquals(part1(example), 5L)

    test("part2"):
        assertEquals(part2(example), 0L)

    test("parseLine"):
        assertEquals(example.head, Device("aaa", List("you", "hhh")))

