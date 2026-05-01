//> using scala 3.8.3
//> using dep com.lihaoyi::pprint:0.9.6
//> using dep org.scalameta::munit:1.3.0

// $ scala-cli test 08.scala

case class Point3D(x: Long, y: Long, z: Long):
    infix def sqDistTo(that: Point3D): Long =
        val dx = this.x - that.x
        val dy = this.y - that.y
        val dz = this.z - that.z
        dx * dx + dy * dy + dz * dz

def part1(jboxes: IndexedSeq[Point3D], nrConnections: Int = 1000): Int =
    pairsByDistanceAsc(jboxes)
        .take(nrConnections)
        .foldLeft(initialCircuits(jboxes.length))(mergeCircuits)
        .map(_.size)
        .sorted(using Ordering[Int].reverse)
        .take(3)
        .product

def part2(jboxes: IndexedSeq[Point3D]): Long =
    val pairs = pairsByDistanceAsc(jboxes)
    val n = pairs
        .iterator
        .scanLeft(initialCircuits(jboxes.length))(mergeCircuits)
        .takeWhile(_.lengthIs > 1)
        .length
    val (i, j) = pairs(n - 1)
    jboxes(i).x * jboxes(j).x

def mergeCircuits(circuits: List[Set[Int]], jboxIndexPair: (Int, Int)): List[Set[Int]] =
    val (p1, p2) = jboxIndexPair
    val both = Set(p1, p2)
    val (before, withP :: after) = circuits.span(!_.exists(both)).runtimeChecked
    if both.subsetOf(withP)
    then circuits
    else before ::: after.map: c =>
        if c.exists(both)
        then c.union(withP)
        else c

def initialCircuits(n: Int): List[Set[Int]] = (0 until n).map(Set(_)).toList

def pairsByDistanceAsc(jboxes: IndexedSeq[Point3D]): Seq[(Int, Int)] =
    val pairsWithDist =
        for i <-       0 until jboxes.length
            j <- (i + 1) until jboxes.length
        yield (i, j)
    pairsWithDist
        .sortBy((i, j) => jboxes(i) sqDistTo jboxes(j))

def parseLine(line: String): Point3D =
    line.split(",").toList.map(_.toLong) match
        case List(x, y, z) => Point3D(x, y, z)
        case _ => throw IllegalArgumentException(line)


class Test08 extends munit.FunSuite:
    val example = IndexedSeq(
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
        val inputLines =
            try src.getLines().filter(_.nonEmpty).to(IndexedSeq)
            finally src.close()
        val jboxes = inputLines.map(parseLine)
        pprint.log(part1(jboxes))
        pprint.log(part2(jboxes))

    test("part1"):
        assertEquals(part1(example, 10), 40)

    test("part2"):
        assertEquals(part2(example), 25272L)

    test("parseLine"):
        assertEquals(example.head, Point3D(162, 817, 812))

    test("pairsByDistanceAsc"):
        val ps = pairsByDistanceAsc(example)
        assert:
            val sqDists = ps.map((i, j) => example(i) sqDistTo example(j))
            sqDists == sqDists.sorted
        assert(ps.forall(_ < _))

    test("mergeCircuits"):
        val cs = List(Set(0), Set(1, 2, 3), Set(4, 5), Set(6), Set(7, 8), Set(9))
        assertEquals(mergeCircuits(cs, (0, 2)), List(Set(0, 1, 2, 3), Set(4, 5), Set(6), Set(7, 8), Set(9)))
        assertEquals(mergeCircuits(cs, (0, 4)), List(Set(1, 2, 3), Set(0, 4, 5), Set(6), Set(7, 8), Set(9)))
        assertEquals(mergeCircuits(cs, (0, 6)), List(Set(1, 2, 3), Set(4, 5), Set(0, 6), Set(7, 8), Set(9)))
        assertEquals(mergeCircuits(cs, (0, 8)), List(Set(1, 2, 3), Set(4, 5), Set(6), Set(0, 7, 8), Set(9)))
        assertEquals(mergeCircuits(cs, (1, 3)), cs)
        assertEquals(mergeCircuits(cs, (2, 7)), List(Set(0), Set(4, 5), Set(6), Set(1, 2, 3, 7, 8), Set(9)))
        assertEquals(mergeCircuits(cs, (5, 8)), List(Set(0), Set(1, 2, 3), Set(6), Set(4, 5, 7, 8), Set(9)))
        assertEquals(mergeCircuits(cs, (7, 8)), cs)
        assertEquals(mergeCircuits(cs, (6, 9)), List(Set(0), Set(1, 2, 3), Set(4, 5), Set(7, 8), Set(6, 9)))

