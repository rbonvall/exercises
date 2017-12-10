// vim: ft=scala

val input = "129,154,49,198,200,133,97,254,41,6,2,1,255,0,191,108"

def shift[T](xs: Seq[T], delta: Int): Seq[T] = {
  val breakpoint = if (delta > 0) delta else xs.length + delta
  xs.drop(breakpoint) ++ xs.take(breakpoint)
}

def blue(n: Any): String = s"${Console.BLUE}$n${Console.RESET}"

case class State(list: Seq[Int], pos: Int, skip: Int) {
  def doIt(length: Int) = {
    val shifted = shift(list, pos)
    val newList = shifted.take(length).reverse.toSeq ++ shifted.drop(length)
    val unshifted = shift(newList, -pos)
    State(unshifted, (pos + length + skip) % list.length, skip + 1)
  }
  override def toString = s"State($show, pos=$pos, skip=$skip)"
  def show = list.map(_.toString).updated(pos, blue(list(pos))).mkString(" ")
}
object State {
  val initial = State((0 to 255).toVector, 0, 0)
}

// Example
List(3, 4, 1, 5)
  .scanLeft (State(0 to 4, 0, 0)) (_ doIt _)
  .foreach(println)

// Part 1
val inputLengths = input.split(",").toVector.map(_.toInt)
val s = inputLengths.foldLeft (State.initial) { (state, length) => state.doIt(length) }
println(s)
println(s.list.take(2).product)

// Part 2
val inputBytes: Vector[Int] = input.map(_.toInt).toVector
val suffix = Vector(17, 31, 73, 47, 23)

def hex(n: Int) = if (n < 16) "0" ++ n.toHexString else n.toHexString

def hash(ns: Seq[Int]): String =
  Stream.fill(64) (ns ++ suffix)
    .flatten
    .foldLeft(State.initial) (_ doIt _)
    .list
    .grouped(16)
    .map { _ reduce { _ ^ _  } }
    .map(hex)
    .mkString

val examples = List(
  ""         -> "a2582a3a0e66e6e86e3812dcb672a272"
, "AoC 2017" -> "33efeb34ea91902bb2f59c9920caa6cd"
, "1,2,3"    -> "3efbe78a8d82f29979031a4aa0b16a9d"
, "1,2,4"    -> "63960835bcdc130f0b66d7ff4f6a5a8e"
)
examples foreach { case (in, expected) =>
  val result = hash(in.map(_.toInt))
  println(s"$expected\t$result\t$in")
}
println(hash(inputBytes))

