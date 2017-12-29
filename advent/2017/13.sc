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

println(severity(input))
