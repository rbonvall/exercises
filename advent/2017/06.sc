// vim: ft=scala

def argmax(banks: Seq[Int]) = banks.zipWithIndex.maxBy(_._1)._2

assert(argmax(Seq(22, 11, 33, 77, 44, 77, 55)) == 3)

def redistribute(banks: Seq[Int]): Seq[Int] = {
  val n = banks.size
  val m = argmax(banks)
  val bm = banks(m)
  val (q, r) = (bm / n, bm % n)
  banks.zipWithIndex.map {
    case (b, i) if i == m               ⇒     q
    case (b, i) if (i - m + n) % n <= r ⇒ b + q + 1
    case (b, i)                         ⇒ b + q
  }
}

val example = Seq(0, 2, 7, 0)
val input = Seq(0, 5, 10, 0, 11, 14, 13, 4, 11, 8, 8, 7, 1, 4, 12, 11)

Stream.iterate(example)(redistribute) take 10 foreach println

val allStates = Stream.iterate(input)(redistribute)
val accumulatedStates =
  allStates
    .scanLeft (Set.empty[Seq[Int]]) { (seen, current) ⇒ seen + current }

val beforeLoop =
  accumulatedStates
    .zipWithIndex
    .takeWhile { case (set, i) ⇒ set.size == i }

val n = beforeLoop.length - 1
println(n)

val firstRepeated = allStates(n)
val loopBeginsAt = allStates.indexWhere { _ == firstRepeated }
println(n - loopBeginsAt)
