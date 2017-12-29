// vim: ft=scala

val input = {
  val src = io.Source.fromFile("13.txt")
  try src.getLines.map { line ⇒
    val ns = line.trim.split(": ").toList.map(_.toInt)
    ns(0) → ns(1)
  }.toMap
  finally src.close()
}

def severity(fw: Map[Int, Int]): Int =
  fw.filter { case (depth, range) ⇒ depth % (2 * (range - 1)) == 0 }
    .map    { case (depth, range) ⇒ depth * range }
    .sum

def findSafeDelay(fw: Map[Int, Int]): Int =
  Iterator.from(0)
    .find { delay ⇒
      val delayedFw = fw.map { case (d, r) ⇒ (d + delay) → r }
      severity(delayedFw) == 0
    }
    .get

val example = Map(0 → 3, 1 → 2, 4 → 4, 6 → 4)
assert(severity(example) == 24)
assert(findSafeDelay(example) == 10)

println(severity(input))
println(findSafeDelay(input))

