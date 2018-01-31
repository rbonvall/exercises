// vim: ft=scala

type State = Symbol
type Direction = Int
val L = -1
val R = +1

def transition(state: State, value: Int): (Int, Direction, State) = (state, value) match {
  case ('A, 0) ⇒ (1, R, 'B) case ('A, 1) ⇒ (1, L, 'E)
  case ('B, 0) ⇒ (1, R, 'C) case ('B, 1) ⇒ (1, R, 'F)
  case ('C, 0) ⇒ (1, L, 'D) case ('C, 1) ⇒ (0, R, 'B)
  case ('D, 0) ⇒ (1, R, 'E) case ('D, 1) ⇒ (0, L, 'C)
  case ('E, 0) ⇒ (1, L, 'A) case ('E, 1) ⇒ (0, R, 'D)
  case ('F, 0) ⇒ (1, R, 'A) case ('F, 1) ⇒ (1, R, 'C)
}

case class MachineState(state: State, tape: Map[Int, Int], cursor: Int) {
  def checksum: Int = tape.values.sum
  def next = {
    val (v, d, s) = transition(state, tape(cursor))
    MachineState(s, tape.updated(cursor, v), cursor + d)
  }
}
object MachineState {
  val emptyTape = Map.empty[Int, Int] withDefaultValue 0
  val initial = MachineState('A, emptyTape, 0)
}

val checksumInterval = 12459852

val c = Iterator.iterate(MachineState.initial)(_.next)
  .drop(checksumInterval)
  .next
  .checksum
println(c)
