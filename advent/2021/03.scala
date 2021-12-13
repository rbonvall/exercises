import scala.io.Source
import java.io.File

def mode[T](s: Seq[T]) =
  s.groupBy(identity).maxBy(_._2.length)._1

extension (s: String)
  def decodeBinary: Long = java.lang.Long.parseLong(s, 2)

def part1(input: List[String]) =
  val encodedGammaRate = input.transpose.map(mode).mkString
  val encodedEpsilonRate = encodedGammaRate
    .replaceAll("0", "x")
    .replaceAll("1", "0")
    .replaceAll("x", "1")
  val γ = encodedGammaRate.decodeBinary
  val ε = encodedEpsilonRate.decodeBinary
  γ * ε

case class BitCriterion(
  bitValueToKeepWhenThereAreMoreZeroes: Char,
  bitValueToKeepWhenThereAreMoreOnes: Char,
  tieBreaker: Char,
)

val oxygenCriterion = BitCriterion('0', '1', '1')
val co2Criterion    = BitCriterion('1', '0', '0')

def iterateRating(input: List[String], bitPos: Int, criterion: BitCriterion): List[String] =
  val bitPosValueCount: Map[Int, Int] = input
    .groupBy(_.charAt(bitPos).toString.toInt)
    .map((k, v) => (k, v.length))
    .withDefaultValue(0)
  val zeros = bitPosValueCount(0)
  val ones  = bitPosValueCount(1)
  val bitValueToKeep: Char =
    if      ones > zeros then criterion.bitValueToKeepWhenThereAreMoreOnes
    else if ones < zeros then criterion.bitValueToKeepWhenThereAreMoreZeroes
    else                      criterion.tieBreaker
  input.filter(_.charAt(bitPos) == bitValueToKeep)

def computeEncodedRating(input: List[String], criterion: BitCriterion): String =
  Iterator
    .iterate((0, input)) { (bitPos, values) =>
      val newValues = iterateRating(values, bitPos, criterion)
      (bitPos + 1, newValues)
    }
    .collect { case (_, List(onlyRemainingListOfValues)) => onlyRemainingListOfValues }
    .next

def part2(input: List[String]) =
  val oxygen = computeEncodedRating(input, oxygenCriterion).decodeBinary
  val co2    = computeEncodedRating(input, co2Criterion   ).decodeBinary
  oxygen * co2

@main
def run =
  val src = Source.fromFile(File("03.txt"))
  val input: List[String] =
    try src.getLines.map(_.toString).toList
    finally src.close()

  assert(mode("abcabcabcbbc") == 'b')

  println(part1(input))
  println(part2(input))
