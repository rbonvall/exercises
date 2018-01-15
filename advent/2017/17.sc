// vim: ft=scala

case class SpinlockState(buffer: Vector[Int], pos: Int) {
  def n = buffer.length
  def stepForwardAndInsert(nrSteps: Int, value: Int) = {
    val advancedPos = (pos + nrSteps) % n
    val newBuffer = buffer.patch(advancedPos + 1, Vector(value), 0)
    val newPos = (advancedPos + 1) % (n + 1)
    copy(newBuffer, newPos)
  }
  def getAt(i: Int) = buffer(i % n)
  def valueAtPos     = getAt(pos)
  def valueAfterPos  = getAt(pos + 1)
  def valueAfterZero = getAt(buffer.indexOf(0) + 1)
  override def toString = {
    buffer
      .map(_.toString)
      .updated(pos, s"(${buffer(pos)})")
      .mkString(" ")
  }
}

object SpinlockState {
  def initial = SpinlockState(Vector(0), 0)
  def iterate(steps: Int): Iterator[SpinlockState] = {
    Iterator.from(1).scanLeft(initial) { (state, i) ⇒ state.stepForwardAndInsert(steps, i) }
  }
}


val example = List("(0)", "0 (1)", "0 (2) 1", "0 2 (3) 1")
assert(SpinlockState.iterate(3).take(4).toList.map(_.toString) startsWith example)

val exampleLastStep = SpinlockState.iterate(3).drop(2017).next
assert(exampleLastStep.valueAfterPos == 638)

val part1 = SpinlockState.iterate(394).drop(2017)    .next.valueAfterPos
println(part1)

val part2 = SpinlockState.iterate(394).drop(50000000).next.valueAfterZero
println(part2)
