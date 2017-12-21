// vim: ft=scala

val input: Seq[Seq[Char]] = {
  val src = io.Source.fromFile("19.txt")
  try src.getLines.toVector.map(_.toVector)
  finally src.close()
}
def get(i: Int, j: Int): Option[Char] =
  input.lift(i).flatMap(_.lift(j))

sealed trait Direction
case object N extends Direction
case object S extends Direction
case object W extends Direction
case object E extends Direction

case class State(row: Int, col: Int, dir: Direction, letters: Seq[Char], done: Boolean) {
  def move = dir match {
    case N => copy(row = row - 1)
    case S => copy(row = row + 1)
    case W => copy(col = col - 1)
    case E => copy(col = col + 1)
  }
  def update =
    get(row, col)
      .map {
        case ' '       => finish
        case '|' | '-' => this
        case '+'       => turn
        case letter    => collect(letter)
      }
      .getOrElse(finish)
  def turn =
    if (dir == N || dir == S) {
      if      (get(row, col - 1) contains '-') copy(dir = W)
      else if (get(row, col + 1) contains '-') copy(dir = E)
      else                       ???
    }
    else {
      if      (get(row - 1, col) contains '|') copy(dir = N)
      else if (get(row + 1, col) contains '|') copy(dir = S)
      else                       ???
    }
  def collect(c: Char) = copy(letters = letters :+ c)
  def finish = copy(done = true)
}

val j = input.head.indexWhere(_ == '|')
val initialState = State(0, j, S, Vector.empty, false)

Iterator.iterate(initialState)(_.move.update).takeWhile(!_.done).zipWithIndex foreach println
