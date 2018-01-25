// vim: ft=scala

val input = {
  val src = io.Source.fromFile("22.txt")
  try src.getLines.map { _.map(_ == '#') }.toVector
  finally src.close()
}
val (height, width) = (input.length, input.head.length)
val infected: Set[(Int, Int)] = (
  for {
    i ← input.indices
    j ← input.head.indices
    row = i - height / 2
    col = j - width  / 2
    if input(i)(j)
  } yield (row, col)
).toSet

sealed trait Direction {
  def turnLeft  = this match { case N ⇒ W case W ⇒ S case S ⇒ E case E ⇒ N }
  def turnRight = this match { case N ⇒ E case E ⇒ S case S ⇒ W case W ⇒ N }
  def reverse   = this match { case N ⇒ S case E ⇒ W case S ⇒ N case W ⇒ E }
  def applyTo(pair: (Int, Int)) = {
    val (i, j) = pair
    this match {
      case N ⇒ (i - 1, j) case W ⇒ (i, j - 1)
      case S ⇒ (i + 1, j) case E ⇒ (i, j + 1)
    }
  }
}
case object N extends Direction
case object S extends Direction
case object W extends Direction
case object E extends Direction

// Part 1

case class Burst(infected: Set[(Int, Int)],
                 current: (Int, Int),
                 dir: Direction,
                 causedInfection: Boolean) {
  val isInfected = infected contains current
  def next = {
    val newDir =
      if (isInfected) dir.turnRight
      else            dir.turnLeft
    val newInfected =
      if (isInfected) infected - current
      else            infected + current
    Burst(newInfected, newDir applyTo current, newDir, !isInfected)
  }
}

object Burst {
  val initial = Burst(infected, (0, 0), N, true)
}

println(
  Iterator.iterate(Burst.initial)(_.next)
    .take(10000)
    .count(_.causedInfection)
)

// Part 2

sealed trait Status
case object Clean    extends Status
case object Weakened extends Status
case object Infected extends Status
case object Flagged  extends Status

val initialGrid = infected
  .map(_ → Infected)
  .toMap
  .withDefaultValue(Clean)

case class EvolvedBurst(grid: Map[(Int, Int), Status],
                        current: (Int, Int),
                        dir: Direction,
                        causedInfection: Boolean) {
  def next = {
    val status = grid(current)
    val newDir = status match {
      case Clean    ⇒ dir.turnLeft
      case Weakened ⇒ dir
      case Infected ⇒ dir.turnRight
      case Flagged  ⇒ dir.reverse

    }
    val newStatus = status match {
      case Clean    ⇒ Weakened
      case Weakened ⇒ Infected
      case Infected ⇒ Flagged
      case Flagged  ⇒ Clean
    }
    EvolvedBurst(grid.updated(current, newStatus),
                 newDir applyTo current,
                 newDir,
                 newStatus == Infected)
  }
}
object EvolvedBurst {
  val initial = EvolvedBurst(initialGrid, (0, 0), N, false)
}

println(
  Iterator.iterate(EvolvedBurst.initial)(_.next)
    .take(10000000)
    .count(_.causedInfection)
)

