//> using scala 3.7.4
//> using dep com.lihaoyi::pprint:0.9.6
//> using dep org.scalameta::munit:1.2.1

// $ scala-cli test 08.scala

case class Point3D(x: Long, y: Long, z: Long):
    def squaredDistTo(that: Point3D): Long =
        val dx = this.x - that.x
        val dy = this.y - that.y
        val dz = this.z - that.z
        dx * dx + dy * dy + dz * dz

def part1(jboxes: List[Point3D], connections: Int = 1000) =
    0L

def part2(jboxes: List[Point3D], connections: Int = 1000) =
    0L

def parseLine(line: String) =
    line.split(",").toList.map(_.toLong) match
        case List(x, y, z) => Point3D(x, y, z)
        case _ => ???

class Test08 extends munit.FunSuite:
    val example = List(
        "162,817,812",
        "57,618,57",
        "906,360,560",
        "592,479,940",
        "352,342,300",
        "466,668,158",
        "542,29,236",
        "431,825,988",
        "739,650,466",
        "52,470,668",
        "216,146,977",
        "819,987,18",
        "117,168,530",
        "805,96,715",
        "346,949,466",
        "970,615,88",
        "941,993,340",
        "862,61,35",
        "984,92,344",
        "425,690,689",
    ).map(parseLine)

    test("run"):
        val src = io.Source.fromFile("08.txt")
        val inputLines = try src.getLines.filter(_.nonEmpty).to(List) finally src.close()
        val jboxes = inputLines.map(parseLine)
        pprint.log(jboxes.length)
        pprint.log(part1(jboxes))
        pprint.log(part2(jboxes))

    test("part1"):
        assertEquals(part1(example, 10), 40L)

    test("part2"):
        assertEquals(part2(example, 10), 0L)

    test("parseLine"):
        assertEquals(example.head, Point3D(162, 817, 812))

