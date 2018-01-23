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
  def applyTo(pair: (Int, Int)) = {
    val (i, j) = pair
    this match {
      case N ⇒ (i - 1, j    )
      case W ⇒ (i    , j - 1)
      case S ⇒ (i + 1, j    )
      case E ⇒ (i    , j + 1)
    }
  }
}
case object N extends Direction
case object S extends Direction
case object W extends Direction
case object E extends Direction

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

