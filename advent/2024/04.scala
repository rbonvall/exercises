//> using scala 3.7.4
//> using dep com.lihaoyi::pprint:0.9.6
//> using dep org.scalameta::munit:1.2.2

// $ scala-cli test 04.scala

def part1(grid: List[String]) =
    val all = List(
        grid,
        gridColumns(grid),
        gridDiagonals(grid),
        gridAntiDiagonals(grid),
    ).flatten
    (all ++ all.map(_.reverse))
        .map: line =>
            xmasPattern.findAllIn(line).map(_ => 1).sum
        .sum

def part2(grid: List[String]) =
    letterACoordinates(grid)
        .filter: (i, j) =>
            1 <= i && i <= grid.head.length - 2 &&
            1 <= j && j <= grid.length - 2
        .count: (i, j) =>
            val nw = grid(i - 1)(j - 1)
            val se = grid(i + 1)(j + 1)
            val ne = grid(i - 1)(j + 1)
            val sw = grid(i + 1)(j - 1)
            val x1 = s"${nw}A${se}"
            val x2 = s"${ne}A${sw}"
            (x1 == "SAM" || x1 == "MAS") &&
            (x2 == "SAM" || x2 == "MAS")

val xmasPattern = "XMAS".r

def gridColumns(grid: List[String]) =
    grid.transpose.map(_.mkString)

def gridDiagonals(grid: List[String]) =
    val height = grid.length
    val paddedLines = grid.zipWithIndex.map: (line, i) =>
        ("." * i) + line + ("." * (height - i - 1))
    gridColumns(paddedLines).map(_.replaceAll("[.]+", ""))

def gridAntiDiagonals(grid: List[String]) =
    gridDiagonals(grid.map(_.reverse))

def letterACoordinates(grid: List[String]): List[(Int, Int)] =
    grid.zipWithIndex.flatMap: (line, i) =>
        line.zipWithIndex.collect { case ('A', j) => (i, j) }

class Test04 extends munit.FunSuite:

    test("run"):
        util.Using(io.Source.fromFile("04.txt")): src =>
            src.getLines.filterNot(_.isEmpty).toList
        .foreach: grid =>
            pprint.log(part1(grid))
            pprint.log(part2(grid))

    val exampleGrid = List(
        "MMMSXXMASM",
        "MSAMXMSMSA",
        "AMXSXMAAMM",
        "MSAMASMSMX",
        "XMASAMXAMM",
        "XXAMMXXAMA",
        "SMSMSASXSS",
        "SAXAMASAAA",
        "MAMMMXMMMM",
        "MXMXAXMASX",
    )

    test("part1"):
        assertEquals(part1(exampleGrid), 18)

    test("part2"):
        assertEquals(part2(exampleGrid), 9)

    val testGrid = List(
        "ABCDE",
        "FGHIJ",
        "KLMNO",
    )

    test("gridColumns"):
        assertEquals(gridColumns(testGrid), List(
            "AFK",
            "BGL",
            "CHM",
            "DIN",
            "EJO",
        ))

    test("gridDiagonals"):
        assertEquals(gridDiagonals(testGrid), List(
            "A",
            "BF",
            "CGK",
            "DHL",
            "EIM",
             "JN",
              "O",
        ))

    test("gridAntiDiagonals"):
        assertEquals(gridAntiDiagonals(testGrid), List(
              "E",
             "DJ",
            "CIO",
            "BHN",
            "AGM",
            "FL",
            "K",
        ))

    test("letterACoordinates"):
        assertEquals(letterACoordinates(exampleGrid).take(3), List(
            (0, 7), (1, 2), (1, 9)
        ))
