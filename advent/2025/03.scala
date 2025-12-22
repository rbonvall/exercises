//> using scala 3.7.4
//> using dep com.lihaoyi::pprint:0.9.6
//> using dep org.scalameta::munit:1.2.1

// $ scala-cli test 03.scala

def part1(joltages: List[String]) =
    joltages
        .map: line =>
            largestJoltage(toDigits(line))
        .sum

def part2(joltages: List[String]) =
    joltages
        .map: line =>
            largestNDigitJoltage(toDigits(line), 12)
                .mkString
                .toLong
        .sum

def toDigits(s: String): List[Int] =
    s.map(_.toString.toInt).to(List)

def largestJoltage(digits: List[Int]): Long =
    val max = digits.init.max
    val i = digits.indexWhere(_ == max)
    val nextMax = digits.drop(i + 1).max
    10 * max + nextMax

def largestNDigitJoltage(digits: List[Int], n: Int): List[Int] =
    if n == 0 then Nil
    else
        val max = digits.dropRight(n - 1).max
        val i = digits.indexWhere(_ == max)
        max :: largestNDigitJoltage(digits.drop(i + 1), n - 1)


class Test03 extends munit.FunSuite:
    val example = List(
        "987654321111111",
        "811111111111119",
        "234234234234278",
        "818181911112111",
    )

    test("run"):
        val src = io.Source.fromFile("03.txt")
        val joltages = try src.getLines.filter(_.nonEmpty).to(List) finally src.close()
        pprint.log(part1(joltages))
        pprint.log(part2(joltages))

    test("part1"):
        assertEquals(part1(example), 357L)

    test("part2"):
        assertEquals(part2(example), 3121910778619L)

    test("toDigits"):
        assertEquals(toDigits("6198"), List(6, 1, 9, 8))

    test("largestJoltage"):
        assertEquals(largestJoltage(toDigits(example(0))), 98L)
        assertEquals(largestJoltage(toDigits(example(1))), 89L)
        assertEquals(largestJoltage(toDigits(example(2))), 78L)
        assertEquals(largestJoltage(toDigits(example(3))), 92L)

    test("largestNDigitJoltage"):
        assertEquals(largestNDigitJoltage(toDigits(example(0)), 12), toDigits("987654321111"))
        assertEquals(largestNDigitJoltage(toDigits(example(1)), 12), toDigits("811111111119"))
        assertEquals(largestNDigitJoltage(toDigits(example(2)), 12), toDigits("434234234278"))
        assertEquals(largestNDigitJoltage(toDigits(example(3)), 12), toDigits("888911112111"))

