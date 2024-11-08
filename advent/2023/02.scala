//> using scala 3.5
//> using dep org.scalameta::munit:1.0.2

case class CubeCount(red: Int, green: Int, blue: Int):
    def power = red * green * blue
    infix def maxCount(that: CubeCount) =
        CubeCount(
            red   = this.red   max that.red,
            green = this.green max that.green,
            blue  = this.blue  max that.blue,
        )

case class Game(id: Int, reveals: List[CubeCount])

@main
def run =
    val src = io.Source.fromFile("02.txt")
    val games =
        try src.getLines
            .filterNot(_.isEmpty)
            .map(parseGame)
            .to(Array)
        finally src.close()

    println(games
        .filter: game =>
            game.reveals.forall: reveal =>
                reveal.red   <= 12 &&
                reveal.green <= 13 &&
                reveal.blue  <= 14
        .map(_.id)
        .sum
    )

    println(games
        .map: game =>
            game.reveals
                .reduce(_ maxCount _)
                .power
        .sum
    )

val colorCountPattern = """(\d+) (red|green|blue)""".r

def extractColorCount(countItems: Array[String], color: String): Int =
    countItems
        .collect:
            case colorCountPattern(n, `color`) => n.toInt
        .headOption
        .getOrElse(0)

def parseReveal(str: String): CubeCount =
    val counts = str.split(", ")
    CubeCount(
        red   = extractColorCount(counts, "red"),
        green = extractColorCount(counts, "green"),
        blue  = extractColorCount(counts, "blue"),
    )

def parseGame(line: String): Game =
    val parts = line.split(": ")
    Game(
        parts(0).dropWhile(!_.isDigit).toInt,
        parts(1).split("; ").map(parseReveal).to(List)
    )


class Tests02 extends munit.FunSuite:

    test("parseReveal"):
        assertEquals(parseReveal("5 blue, 1 red, 4 green"), CubeCount(red = 1, green = 4, blue = 5))
        assertEquals(parseReveal("2 green"),                CubeCount(red = 0, green = 2, blue = 0))

    test("parseGame"):
        val line = "Game 48: 4 green, 5 red; 19 green, 1 blue, 11 red; 4 red, 8 green; 10 red, 1 blue, 16 green"
        assertEquals(parseGame(line), Game(48, List(
            CubeCount(red =  5, green =  4, blue =  0),
            CubeCount(red = 11, green = 19, blue =  1),
            CubeCount(red =  4, green =  8, blue =  0),
            CubeCount(red = 10, green = 16, blue =  1),
        )))

