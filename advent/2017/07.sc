// vim: ft=scala

case class Program(name: String, weight: Int, above: Seq[String])

def lineToProgram(line: String) = {
  val parts = line.replaceAll("[()]", "").split(" -> ").toSeq
  val left = parts(0).split(" ")
  val above =
    if (parts.length == 1) Seq.empty
    else                   parts(1).split(", ").toSeq
  Program(left(0), left(1).toInt, above)
}

val input = scala.io.Source.fromFile("7.txt").getLines.map(lineToProgram).toVector
val allAbove = input.flatMap(_.above).toSet
val all = input.map(_.name).toSet

all diff allAbove foreach println

val byName = input.map { p ⇒ p.name → p }.toMap
def weightOnTop(name: String): Int =
  byName(name).above.map(weightOnTop).sum + byName(name).weight

val byTotalWeightOnTop =
  input.map { p ⇒ p.name → weightOnTop(p.name) }.toMap

//byTotalWeightOnTop foreach println
byTotalWeightOnTop foreach println

input.find(_.above.map(weightOnTop).toSet.size > 1) foreach { p ⇒
  println(p)
  println(p.above.map(byTotalWeightOnTop))
  p.above.map(byName) foreach println
}

