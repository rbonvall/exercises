extension (c: Char)
  def isOpening =
    c == '(' || c == '[' || c == '{' || c == '<'
  def opposite = c match
    case '(' => ')' case ')' => '('
    case '[' => ']' case ']' => '['
    case '<' => '>' case '>' => '<'
    case '{' => '}' case '}' => '{'
    case x => x

case class State(charStack: List[Char]):
  def update(c: Char): Option[State] = // None if corrupt
    if c.isOpening then Some(State(c :: charStack))
    else charStack match
      case x :: xs if x == c.opposite => Some(State(xs))
      case _ => None

  override def toString =
    charStack.mkString("State# ", " ", " #")

def syntaxErrorScore(c: Char) = c match
  case ')' => 3
  case ']' => 57
  case '}' => 1197
  case '>' => 25137
  case _   => 0

def completionScore(c: Char) = c match
  case ')' => 1
  case ']' => 2
  case '}' => 3
  case '>' => 4
  case _   => 0

def syntaxErrorScoreForLine(line: String): Int =
  val states = line.scanLeft(Option(State(Nil))) { (state, char) =>
    state.flatMap(_.update(char))
  }
  val i = states.indexWhere(_.isEmpty) - 1
  if i >= 0 then syntaxErrorScore(line(i)) else 0

def completionScoreForLine(line: String): Long =
  val finalState = line.foldLeft(Option(State(Nil))) { (state, char) =>
    state.flatMap(_.update(char))
  }
  finalState match
    case Some(State(stack)) =>
      stack.foldLeft(0L) { (total, c) =>
        total * 5L + completionScore(c.opposite)
      }
    case None => 0L


def part1(lines: List[String]) =
  lines.map(syntaxErrorScoreForLine).sum

def part2(lines: List[String]) =
  val scores = lines.map(completionScoreForLine).filter(_ > 0).sorted
  scores(scores.length / 2)

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
  assert(part2(example) == 288957)

  println(part1(input))
  println(part2(input))

