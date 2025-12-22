//> using scala 3.7.4
//> using dep com.lihaoyi::pprint:0.9.6
//> using dep org.scalameta::munit:1.2.1

// $ scala-cli test 04.scala

class Grid(val lines: Seq[String]):
    val width = lines.head.length
    val height = lines.length

    // Enclose the grid between pairs of blank rows and columns
    // to make it easier to collect all eight neighbors.
    val padded =
        val paddedEmptyRow = " " * (width + 2)
        paddedEmptyRow +: lines.map(l => s" $l ") :+ paddedEmptyRow

    def pos(i: Int, j: Int): Char = lines(i)(j)

    def adjacentPositions(i: Int, j: Int): String =
        padded(i    ).slice(j,     j + 3) + // three on top
        padded(i + 1).slice(j,     j + 1) + // one on left side
        padded(i + 1).slice(j + 2, j + 3) + // one on right side
        padded(i + 2).slice(j,     j + 3)   // three on bottom

    def indices: Iterable[(Int, Int)] =
        for i <- 0 until height
            j <- 0 until width
        yield (i, j)

    def isAccessibleRoll(i: Int, j: Int): Boolean =
        pos(i, j) == '@' && adjacentPositions(i, j).count(_ == '@') < 4

    lazy val accessibleRolls: Seq[(Int, Int)] =
        indices.filter(isAccessibleRoll).to(Seq)

    def remove(indices: Seq[(Int, Int)]): Grid =
        val byLine: Map[Int, Seq[Int]] = indices.groupBy(_._1)
            .mapValues(_.map(_._2))
            .toMap
        val newLines = lines.zipWithIndex.map: (line, i) =>
            byLine.get(i) match
                case Some(indices) => replaceIndices(line, indices, '.')
                case None          => line
        Grid(newLines)


def part1(grid: Grid) =
    grid.indices.count(grid.isAccessibleRoll)

def part2(grid: Grid) =
    LazyList.iterate(grid)(g => g.remove(g.accessibleRolls))
        .map(_.accessibleRolls.length)
        .takeWhile(_ > 0)
        .sum

def replaceIndices(string: String, indices: Seq[Int], replacement: Char): String =
    val chars = string.toCharArray
    indices.foreach: i =>
        if 0 <= i && i < chars.length
        then chars(i) = replacement
    chars.mkString


class Test04 extends munit.FunSuite:
    val example = Grid(List(
        "..@@.@@@@.",
        "@@@.@.@.@@",
        "@@@@@.@.@@",
        "@.@@@@..@.",
        "@@.@@@@.@@",
        ".@@@@@@@.@",
        ".@.@.@.@@@",
        "@.@@@.@@@@",
        ".@@@@@@@@.",
        "@.@.@@@.@.",
    ))

    test("run"):
        val src = io.Source.fromFile("04.txt")
        val gridLines = try src.getLines.filter(_.nonEmpty).to(List) finally src.close()
        val grid = Grid(gridLines)
        pprint.log(part1(grid))
        pprint.log(part2(grid))

    test("part1"):
        assertEquals(part1(example), 13)

    test("part2"):
        assertEquals(part2(example), 43)

    test("Grid#adjacentPositions"):
        assertEquals(example.adjacentPositions(0, 0), "    . @@")
        assertEquals(example.adjacentPositions(1, 1), "..@@@@@@")
        assertEquals(example.adjacentPositions(9, 9), "@. @    ")

    test("Grid#remove"):
        val input = Grid(List(
        //   012345
            "xxxxxx", // 0
            "xxxxxx", // 1
            "xxxxxx", // 2
        ))
        val result = input.remove(Seq((2, 4), (0, 3), (2, 1), (9, 9)))
        val expected = Grid(List(
        //   012345
            "xxx.xx", // 0
            "xxxxxx", // 1
            "x.xx.x", // 2
        ))

