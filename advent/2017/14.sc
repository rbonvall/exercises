// vim: ft=scala

// must be run with ammonite for this import to work
import $file.`10`

val key = "hxtvlmkl"
val inputs = 0 to 127 map { i => s"$key-$i".map(_.toInt).toVector }
val hashes = inputs.map(`10`.hash)

def hexToBin(c: Char): Seq[Int] = {
  val bits = "0123456789abcdef".indexOf(c).toBinaryString
  ("0" * (4 - bits.length) ++ bits).map(_.toString.toInt)
}

def hashToBinary(s: String): Seq[Int] = s.flatMap(hexToBin)

val matrix: Seq[Seq[Int]] = hashes.map(hashToBinary)
//println(hashes.flatten.sum)
//matrix map (_.mkString) foreach println

case class Coord(i: Int, j: Int) {
  def neighbors = Set(
    copy(i = i + 1), copy(i = i - 1),
    copy(j = j + 1), copy(j = j - 1)
  ).filter { case Coord(i, j) =>
    0 <= i && i <= 127 &&
    0 <= j && j <= 127
  }
}

val coords: Set[Coord] = (
  for {
    i <- 0 to 127
    j <- 0 to 127
    if matrix(i)(j) == 1
  } yield Coord(i, j)
).toSet

assert(matrix.flatten.sum == coords.size)
println(s"Total of used squares: ${coords.size}")

def traverseFrom(c: Coord, visited: Set[Coord] = Set.empty, depth: Int = 0): Set[Coord] = {
  //val toVisit = c.neighbors.filter(coords.contains).diff(visited)
  //toVisit.flatMap { n => traverseFrom(n, visited + c, depth + 1) } + c
  c.neighbors
    .filter(coords.contains)
    .diff(visited)
    .foldLeft (Set(c)) { case (thisGroup, n) =>
      //println(" " * depth + n.toString)
      if (thisGroup contains n) thisGroup
      else                      thisGroup ++ traverseFrom(n, visited ++ thisGroup, depth + 1)
    }
}

def countAllRegions(remaining: Set[Coord], acc: Int): Int =
  if (remaining.isEmpty) acc
  else {
    val startFrom = remaining.minBy { case Coord(i, j) => (i, j) }
    //println(s"Starting from: $startFrom")
    val nextRegion = traverseFrom(startFrom)
    //println(nextRegion.toSeq.sortBy { case Coord(i, j) => (i, j) }.mkString("{", ",", "}"))
    //println("-" * 40)
    countAllRegions(remaining diff nextRegion, acc + 1)
  }

println(countAllRegions(coords, 0))
