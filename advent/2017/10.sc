// vim: ft=scala

val input =
  "129,154,49,198,200,133,97,254,41,6,2,1,255,0,191,108"
  .split(",")
  .toVector
  .map(_.toInt)

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
val s = input.foldLeft (State.initial) { (state, length) => state.doIt(length) }
println(s)
println(s.list.take(2).product)

