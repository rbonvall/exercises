//> using scala 3.5
//> using dep org.scalameta::munit:1.0.2
//> using option -deprecation

case class Card(winningNumbers: List[Int], myNumbers: List[Int]):
    val matching = winningNumbers.intersect(myNumbers).length
    val worth = matching match
        case 0 => 0
        case n => 1 << (n - 1)

object Card:
    def parseNumbers(text: String) =
        text.trim.split("[ ]+").map(_.toInt).to(List)

    def parse(line: String): Card =
        val parts = line.split("[:|]")
        Card(parseNumbers(parts(1)), parseNumbers(parts(2)))


object Process:
    case class Step(nrCards: Int, nextCopiesToAdd: List[Int]):
        def nextStep(m: Int): Step =
            val nc = 1 + nextCopiesToAdd.headOption.getOrElse(0)

            val (nextM, rest) = nextCopiesToAdd.drop(1).splitAt(m)
            Step(nc, nextM.padTo(m, 0).map(_ + nc) ++ rest)

    val stepZero = Step(0, Nil)
    def allSteps(cards: Iterable[Card]): List[Step] =
         cards.map(_.matching)
             .scanLeft(stepZero)(_.nextStep(_))
             .tail.to(List)

@main
def run =
    val src = io.Source.fromFile("04.txt")
    val cards =
        try src.getLines
            .filter(_.nonEmpty)
            .map(Card.parse)
            .to(Array)
        finally src.close()

    println(part1(cards))
    println(part2(cards))

def part1(cards: Iterable[Card]) =
    cards.map(_.worth).sum

def part2(cards: Iterable[Card]) =
    Process.allSteps(cards).map(_.nrCards).sum


class Tests04 extends munit.FunSuite:

    val exampleLines =
        """Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
          |Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
          |Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
          |Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
          |Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
          |Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11""".stripMargin.split("\n")

    lazy val exampleCards = exampleLines.map(Card.parse).to(Array)

    test("Card.parse"):
        assertEquals(
            exampleCards.head,
            Card(List(41, 48, 83, 86, 17), List(83, 86, 6, 31, 17, 9, 48, 53))
        )

    test("Card#worth"):
        assertEquals(exampleCards(0).worth, 8)
        assertEquals(exampleCards(1).worth, 2)
        assertEquals(exampleCards(2).worth, 2)
        assertEquals(exampleCards(3).worth, 1)
        assertEquals(exampleCards(4).worth, 0)
        assertEquals(exampleCards(5).worth, 0)

    test("Process.allSteps"):
        assertEquals(Process.allSteps(exampleCards), List(
            Process.Step( 1, List(1, 1, 1, 1)),
            Process.Step( 2, List(   3, 3, 1)),
            Process.Step( 4, List(      7, 5)),
            Process.Step( 8, List(        13)),
            Process.Step(14, List(          )),
            Process.Step( 1, List(          )),
        ))

    test("part1"):
        assertEquals(part1(exampleCards), 13)

    test("part2"):
        assertEquals(part2(exampleCards), 30)
