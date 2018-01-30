// vim: ft=scala

def parsePattern(pattern: String): Array[Int] = {
  pattern.split("/").flatMap { _.map { case '.' ⇒ 0 case '#' ⇒ 1 } }
}

def parseLine(line: String): (Array[Int], Array[Int]) = {
  val parts = line.split(" => ")
  parsePattern(parts(0)) → parsePattern(parts(1))
}

val input = {
  val source = io.Source.fromFile("21.txt")
  try source.getLines.map(parseLine).toVector
  finally source.close()
}

val (rules2, rules3) = input.partition { case (from, to) ⇒ from.size == 4 }

def variants(g: Array[Int]) = {
  val g90  = g   .rotate90
  val g180 = g90 .rotate90
  val g270 = g180.rotate90
  val rots = Seq(g, g90, g180, g270)
  rots ++ rots.map(_.flipV)
}

val allRules2 = rules2.flatMap { case (from, to) ⇒ variants(from).map(_ → to) }.toMap
val allRules3 = rules3.flatMap { case (from, to) ⇒ variants(from).map(_ → to) }.toMap

implicit class Grid(data: Array[Int]) {
  val n = math.sqrt(data.length).toInt
  val n_2 = n / 2
  val n_3 = n / 3
  def split2: Seq[Array[Int]] =
    for { m ← 0 until data.length/4 } yield {
      val z = new Array[Int](4)
      val i = 2 * (n * (m / n_2) + m % n_2)
      z(0) = data(i)    ; z(1) = data(i + 1)
      z(2) = data(i + n); z(3) = data(i + n + 1)
      z
    }
  def split3: Seq[Array[Int]] =
    for { m ← 0 until data.length/9 } yield {
      val z = new Array[Int](9)
      val i = 3 * (n * (m / n_3) + m % n_3)
      val nn = 2 * n
      z(0) = data(i     ); z(1) = data(i      + 1); z(2) = data(i      + 2)
      z(3) = data(i +  n); z(4) = data(i +  n + 1); z(5) = data(i +  n + 2)
      z(6) = data(i + nn); z(7) = data(i + nn + 1); z(8) = data(i + nn + 2)
      z
    }
  def rotate90: Array[Int] =
    if (n == 2) {
      val z = new Array[Int](4)
      z(0) = data(1); z(1) = data(3)
      z(2) = data(0); z(3) = data(2)
      z
    }
    else if (n == 3) {
      val z = new Array[Int](9)
      z(0) = data(2); z(1) = data(5); z(2) = data(8)
      z(3) = data(1); z(4) = data(4); z(5) = data(7)
      z(6) = data(0); z(7) = data(3); z(8) = data(6)
      z
    }
    else ???
  def rotate180 = rotate90.rotate90
  def rotate270 = rotate90.rotate180
  def flipV: Array[Int] =
    if (n == 2) {
      val z = new Array[Int](4)
      z(0) = data(2); z(1) = data(3)
      z(2) = data(0); z(3) = data(1)
      z
    }
    else if (n == 3) {
      val z = new Array[Int](9)
      z(0) = data(6); z(1) = data(7); z(2) = data(8)
      z(3) = data(3); z(4) = data(4); z(5) = data(5)
      z(6) = data(0); z(7) = data(1); z(8) = data(2)
      z
    }
    else ???
  def flipH = rotate90.flipV.rotate270
  def repr: String =
    (0 until n)
      .map { i ⇒ data.slice(i * n, (i + 1) * n).map(_.toString).mkString }
      .mkString("|")
}

def exampleGrid(n: Int) = (0 until (n * n)).toArray

def compare(grids1: Seq[Array[Int]], grids2: Seq[Array[Int]]) = {
  grids1 zip grids2 foreach { case (g1, g2) ⇒ assert(g1.deep == g2.deep, s"${g1.deep} != ${g2.deep}")}
}
val g4_2 = Seq(
  Array( 0,  1,   4,  5), Array( 2,  3,   6,  7),
  Array( 8,  9,  12, 13), Array(10, 11,  14, 15))
compare(exampleGrid(4).split2, g4_2)
val g6_2 = Seq(
  Array( 0,  1,   6,  7), Array( 2,  3,   8,  9), Array( 4,  5,  10, 11),
  Array(12, 13,  18, 19), Array(14, 15,  20, 21), Array(16, 17,  22, 23),
  Array(24, 25,  30, 31), Array(26, 27,  32, 33), Array(28, 29,  34, 35))
compare(exampleGrid(6).split2, g6_2)
val g6_3 = Seq(
  Array( 0,  1,  2,   6,  7,  8,  12, 13, 14), Array( 3,  4,  5,   9, 10, 11,  15, 16, 17),
  Array(18, 19, 20,  24, 25, 26,  30, 31, 32), Array(21, 22, 23,  27, 28, 29,  33, 34, 35))
compare(exampleGrid(6).split3, g6_3)
val g8_2 = Seq(
  Array( 0,  1,   8,  9), Array( 2,  3,  10, 11), Array( 4,  5,  12, 13), Array( 6,  7,  14, 15),
  Array(16, 17,  24, 25), Array(18, 19,  26, 27), Array(20, 21,  28, 29), Array(22, 23,  30, 31),
  Array(32, 33,  40, 41), Array(34, 35,  42, 43), Array(36, 37,  44, 45), Array(38, 39,  46, 47),
  Array(48, 49,  56, 57), Array(50, 51,  58, 59), Array(52, 53,  60, 61), Array(54, 55,  62, 63))
compare(exampleGrid(8).split2, g8_2)
val g9_3 = Seq(
  Array( 0,  1,  2,   9, 10, 11,  18, 19, 20), Array( 3,  4,  5,  12, 13, 14,  21, 22, 23), Array( 6,  7,  8,  15, 16, 17,  24, 25, 26),
  Array(27, 28, 29,  36, 37, 38,  45, 46, 47), Array(30, 31, 32,  39, 40, 41,  48, 49, 50), Array(33, 34, 35,  42, 43, 44,  51, 52, 53),
  Array(54, 55, 56,  63, 64, 65,  72, 73, 74), Array(57, 58, 59,  66, 67, 68,  75, 76, 77), Array(60, 61, 62,  69, 70, 71,  78, 79, 80))
compare(exampleGrid(9).split3, g9_3)
