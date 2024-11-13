//> using scala 3.5
//> using dep org.scalameta::munit:1.0.2
//> using option -deprecation

enum RowItem(val width: Int):
    case Dots(howMany: Int) extends RowItem(howMany)
    case Num(n: String)     extends RowItem(n.length)
    case Sym(c: Char)       extends RowItem(1)

case class Row(items: List[RowItem]):
    val itemsWithStartingIndex = items zip items.scanLeft(0)(_ + _.width)

    val numbersWithRange: List[(Range, Int)] = itemsWithStartingIndex.collect:
         case (RowItem.Num(n), j0) => (j0 until (j0 + n.length), n.toInt)

    val symbolsWithIndex: List[(Int, Char)] = itemsWithStartingIndex.collect:
         case (RowItem.Sym(c), j) => (j, c)

    def symbolsBetween(start: Int, end: Int): List[Char] =
        symbolsWithIndex.collect:
            case (j, c) if start to end contains j => c

    def numbersBetween(start: Int, end: Int): List[Int] =
        numbersWithRange.collect:
            case (nr, n) if (start to end intersect nr).nonEmpty => n

case class Schematic(rows: IndexedSeq[Row])


@main
def run =
    val src = io.Source.fromFile("03.txt")
    val lines =
        try src.getLines
            .filter(_.nonEmpty)
            .to(Array)
        finally src.close()

    val schematic = parseSchematic(lines)
    println(part1(schematic))
    println(part2(schematic))

val startsWithDigits = "([0-9]+)(.*)".r
val startsWithDots   = "([.]+)(.*)".r

def parseRowLine(line: String): Row =
    @annotation.tailrec
    def splitRec(line: String, acc: List[RowItem]): List[RowItem] =
        line match
            case "" => acc.reverse
            case startsWithDigits(digits, rest) => splitRec(rest,   RowItem.Num(digits)       :: acc)
            case startsWithDots  (dots,   rest) => splitRec(rest,   RowItem.Dots(dots.length) :: acc)
            case s                              => splitRec(s.tail, RowItem.Sym(s.head)       :: acc)
    Row(splitRec(line, Nil))

def parseSchematic(lines: Iterable[String]): Schematic =
    Schematic(lines.map(parseRowLine).to(IndexedSeq))

def part1(s: Schematic) =
    val partNumbers =
        for (row, i) <- s.rows.zipWithIndex
            surroundingRows = s.rows.slice(i - 1, i + 2)
            (nRange, n) <- row.numbersWithRange
            adjacentSyms = surroundingRows.flatMap(_.symbolsBetween(nRange.head - 1, nRange.last + 1))
            if adjacentSyms.nonEmpty
        yield n

    partNumbers.sum


def part2(s: Schematic) =
    val gearNumberPairs =
        for (row, i) <- s.rows.zipWithIndex
            surroundingRows = s.rows.slice(i - 1, i + 2)
            case (j, '*') <- row.symbolsWithIndex
            adjacentNumbers = surroundingRows.flatMap(_.numbersBetween(j - 1, j + 1))
            if adjacentNumbers.length == 2
            n1 = adjacentNumbers(0)
            n2 = adjacentNumbers(1)
        yield (n1, n2)

    gearNumberPairs.map(_ * _).sum


class Tests03 extends munit.FunSuite:
    import RowItem.*

    val example =
        """467..114..
          |...*......
          |..35..633.
          |......#...
          |617*......
          |.....+.58.
          |..592.....
          |......755.
          |...$.*....
          |.664.598..""".stripMargin

    lazy val exampleSchematic = parseSchematic(example.split("\n"))

    test("parseRowLine"):
        assertEquals(
            parseRowLine("12..3*...456.%$..***89"),
            Row(List(
                Num("12"),
                Dots(2),
                Num("3"),
                Sym('*'),
                Dots(3),
                Num("456"),
                Dots(1),
                Sym('%'),
                Sym('$'),
                Dots(2),
                Sym('*'),
                Sym('*'),
                Sym('*'),
                Num("89"),
            ))
        )

    test("parseSchematic"):
        assertEquals(
            exampleSchematic,
            Schematic(IndexedSeq(
                Row(List(Num("467"), Dots(2), Num("114"), Dots(2))),
                Row(List(Dots(3), Sym('*'), Dots(6))),
                Row(List(Dots(2), Num("35"), Dots(2), Num("633"), Dots(1))),
                Row(List(Dots(6), Sym('#'), Dots(3))),
                Row(List(Num("617"), Sym('*'), Dots(6))),
                Row(List(Dots(5), Sym('+'), Dots(1), Num("58"), Dots(1))),
                Row(List(Dots(2), Num("592"), Dots(5))),
                Row(List(Dots(6), Num("755"), Dots(1))),
                Row(List(Dots(3), Sym('$'), Dots(1), Sym('*'), Dots(4))),
                Row(List(Dots(1), Num("664"), Dots(1), Num("598"), Dots(2))),
            ))
        )

    test("part1"):
        assertEquals(part1(exampleSchematic), 4361)

    test("part2"):
        assertEquals(part2(exampleSchematic), 467835)


