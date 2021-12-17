extension (c: Char)
  def isOpening =
    c == '(' || c == '[' || c == '{' || c == '<'
  def isOppositeOf(d: Char) = (c min d, c max d) match
    case ('(', ')') => true
    case ('[', ']') => true
    case ('{', '}') => true
    case ('<', '>') => true
    case _          => false

case class State(charStack: List[Char]):
  def update(c: Char): Option[State] = // None if corrupt
    if c.isOpening then Some(State(c :: charStack))
    else charStack match
      case x :: xs if c isOppositeOf x => Some(State(xs))
      case _ => None

  override def toString =
    charStack.mkString("State# ", " ", " #")

def score(c: Char) = c match
  case ')' => 3
  case ']' => 57
  case '}' => 1197
  case '>' => 25137
  case _   => 0

def scoreForLine(line: String): Int =
  val states = line.scanLeft(Option(State(Nil))) { (state, char) =>
    state.flatMap(_.update(char))
  }
  val i = states.indexWhere(_.isEmpty) - 1
  if i >= 0 then score(line(i)) else 0

def part1(lines: List[String]) =
  lines.map(scoreForLine).sum

def part2(lines: List[String]) =
  ()

@main
def run =
  val src = io.Source.fromFile("10.txt")
  val input: List[String] =
    try src.getLines.filter(_.nonEmpty).toList
    finally src.close()

  val example =
    """[({(<(())[]>[[{[]{<()<>>
      |[(()[<>])]({[<{<<[]>>(
      |{([(<{}[<>[]}>{[]{[(<()>
      |(((({<>}<{<{<>}{[]{[]{}
      |[[<[([]))<([[{}[[()]]]
      |[{[{({}]{}}([{[{{{}}([]
      |{<[[]]>}<{[{[{[]{()[[[]
      |[<(<(<(<{}))><([]([]()
      |<{([([[(<>()){}]>(<<{{
      |<{([{{}}[<[[[<>{}]]]>[]]"""
      .trim
      .stripMargin('|')
      .linesIterator
      .toList

  assert(part1(example) == 26397)

  println(part1(input))
  println(part2(input))

