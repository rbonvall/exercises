// vim: ft=scala

sealed trait Move
case class Spin    (x: Int)           extends Move
case class Exchange(a: Int,  b: Int)  extends Move
case class Partner (a: Char, b: Char) extends Move
object Move {
  val spin     = """s(\d+)""".r
  val exchange = """x(\d+)/(\d+)""".r
  val partner  = """p(\w)/(\w)""".r
  val fromString: PartialFunction[String, Move] = {
    case spin    (x)    ⇒ Spin    (x.toInt)
    case exchange(a, b) ⇒ Exchange(a.toInt, b.toInt)
    case partner (a, b) ⇒ Partner (a.head,  b.head)
  }
}

val input = {
  val src = scala.io.Source.fromFile("16.txt")
  try src.mkString.split(",").toVector.map(_.trim).map(Move.fromString)
  finally src.close()
}

val initialState = ('a' to 'p').zipWithIndex.toMap

def applyMove(state: Map[Char, Int], move: Move): Map[Char, Int] = move match {
  case Spin(x) ⇒
    val n = state.size
    state.mapValues { v ⇒ (v + x) % n }
  case Exchange(posA, posB) ⇒
    val progA = state.collect { case (k, `posA`) ⇒ k }.head
    val progB = state.collect { case (k, `posB`) ⇒ k }.head
    state.updated(progA, posB).updated(progB, posA)
  case Partner(progA, progB) ⇒
    val posA = state(progA)
    val posB = state(progB)
    state.updated(progA, posB).updated(progB, posA)
}

def doOneDance(moves: Seq[Move])(initialState: Map[Char, Int]): Map[Char, Int] =
  moves.foldLeft(initialState)(applyMove)

def stateToString(state: Map[Char, Int]) =
  state.toSeq.sortBy(_._2).map(_._1).mkString

val afterOneDance = doOneDance(input)(initialState)
println(stateToString(afterOneDance))
println(stateToString {
  Iterator.iterate(initialState)(doOneDance(input)).drop(1).next
})

val afterOneBillionDances =
  Iterator.iterate(initialState)(doOneDance(input)).drop(1000000000).next
println(stateToString(afterOneBillionDances))
