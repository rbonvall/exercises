//> using scala 3.5
//> using dep org.scalameta::munit:1.0.2

@main
def run =
    val src = io.Source.fromFile("01.txt")
    val lines =
        try src.getLines.filterNot(_.isEmpty).to(Array)
        finally src.close()
    println(lines.map(extractPart1).sum)
    println(lines.map(extractPart2).sum)

def extractPart1(line: String): Int =
    val digits = line.filter(_.isDigit).map(_.toString.toInt)
    10 * digits.head + digits.last

def extractPart2(line: String): Int =
    val tenths = digitsFromLeft (line).head
    val units  = digitsFromRight(line).head
    10 * tenths + units

def digitsFromLeft(line: String): Iterable[Int] =
    for tail <- line.tails.to(LazyList)
        (name, value) <- digitNameValues.find((n, v) => tail.startsWith(n))
    yield value

def digitsFromRight(line: String): Iterable[Int] =
    for init <- line.inits.to(LazyList)
        (name, value) <- digitNameValues.find((n, v) => init.endsWith(n))
    yield value

val digitNameValues = List(
    "0" -> 0, "zero"  -> 0,
    "1" -> 1, "one"   -> 1,
    "2" -> 2, "two"   -> 2,
    "3" -> 3, "three" -> 3,
    "4" -> 4, "four"  -> 4,
    "5" -> 5, "five"  -> 5,
    "6" -> 6, "six"   -> 6,
    "7" -> 7, "seven" -> 7,
    "8" -> 8, "eight" -> 8,
    "9" -> 9, "nine"  -> 9,
)


class Tests01 extends munit.FunSuite:
    test("extractPart1"):
        for (input, expected) <- List(
            "1abc2"       ->  12,
            "pqr3stu8vwx" ->  38,
            "a1b2c3d4e5f" ->  15,
            "treb7uchet"  ->  77,
        )
        do assertEquals(extractPart1(input), expected)

    test("extractPart2"):
        for (input, expected) <- List(
            "two1nine"         -> 29,
            "eightwothree"     -> 83,
            "abcone2threexyz"  -> 13,
            "xtwone3four"      -> 24,
            "4nineeightseven2" -> 42,
            "zoneight234"      -> 14,
            "7pqrstsixteen"    -> 76,
        )
        do assertEquals(extractPart2(input), expected)

