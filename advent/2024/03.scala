//> using scala 3.7.4
//> using dep com.lihaoyi::pprint:0.9.6
//> using dep org.scalameta::munit:1.2.2

// $ scala-cli test 03.scala

val mulPattern = """mul[(]\d{1,3},\d{1,3}[)]""".r

def part1(memory: String) =
    mulPattern.findAllIn(memory)
        .map(mulArgs)
        .map(_ * _)
        .sum

val instrPattern = """mul\(\d{1,3},\d{1,3}\)|do\(\)|don't\(\)""".r

@annotation.tailrec
def keepEnabled(instructions: List[String], acc: List[String] = Nil): List[String] =
    instructions match
        case Nil             => acc
        case "don't()" :: is => keepEnabled(is.dropWhile(_ != "do()"), acc)
        case "do()"    :: is => keepEnabled(is, acc)
        case instr     :: is => keepEnabled(is, instr :: acc)

def part2(memory: String) =
    keepEnabled(instrPattern.findAllIn(memory).toList)
        .map(mulArgs)
        .map(_ * _)
        .sum

def mulArgs(mul: String): (Int, Int) =
    val nums = mul.split("[^0-9]+").slice(1, 3)
    (nums(0).toInt, nums(1).toInt)

class Test03 extends munit.FunSuite:

    test("run"):
        val src = io.Source.fromFile("03.txt")
        val memory =
            try src.mkString
            finally src.close()

        pprint.log(part1(memory))
        pprint.log(part2(memory))

    test("part1"):
        val mem = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
        assertEquals(part1(mem), 161)

    test("part2"):
        val mem = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
        assertEquals(part2(mem), 48)

    test("mulArgs"):
        assertEquals(mulArgs("mul(12,34)"), (12, 34))

