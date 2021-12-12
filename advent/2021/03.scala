import scala.io.Source
import java.io.File

def mode[T](s: Seq[T]) =
  s.groupBy(identity).maxBy(_._2.length)._1

def part1(input: List[String]) =
  val encodedGammaRate = input.transpose.map(mode).mkString
  val encodedEpsilonRate = encodedGammaRate
    .replaceAll("0", "x")
    .replaceAll("1", "0")
    .replaceAll("x", "1")
  val γ = java.lang.Long.parseLong(encodedGammaRate, 2)
  val ε = java.lang.Long.parseLong(encodedEpsilonRate, 2)
  γ * ε


def part2(input: List[String]) =
  ()

@main
def run =
  val src = Source.fromFile(File("03.txt"))
  val input: List[String] =
    try src.getLines.map(_.toString).toList
    finally src.close()

  println(part1(input))
  println(part2(input))
