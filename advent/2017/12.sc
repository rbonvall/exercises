// vim: ft=scala

val source = io.Source.fromFile("12.txt")
val lines = try source.getLines.toVector finally source.close()
val neighbors: Map[Int, Set[Int]] =
  lines.map { line ⇒
    val parts = line.trim.split("""\D+""").map(_.toInt)
    parts.head -> parts.tail.toSet
  }.toMap

def traverseFrom(current: Int, visited: Set[Int] = Set.empty): Set[Int] = {
  neighbors(current)
    .diff(visited)
    .flatMap { n => traverseFrom(n, visited + current) } + current
}

val group0 = traverseFrom(0)
println(group0.size)


def countAllGroups(remaining: Set[Int], acc: Int): Int =
  if (remaining.isEmpty) acc
  else countAllGroups(remaining diff traverseFrom(remaining.head), acc + 1)

println(countAllGroups(neighbors.keySet, 0))



