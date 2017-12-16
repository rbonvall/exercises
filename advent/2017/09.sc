// vim: ft=scala

def normalize(input: String) =
  input.trim
    .replaceAll("[!].", "")
    .replaceAll("<[^>]*>", "")

val examples = List(
  "{}" -> 1
, "{{{}}}" -> 6
, "{{},{}}" ->  5
, "{{{},{},{{}}}}" -> 16
, "{<a>,<a>,<a>,<a>}" -> 1
, "{{<ab>},{<ab>},{<ab>},{<ab>}}" -> 9
, "{{<!!>},{<!!>},{<!!>},{<!!>}}" -> 9
, "{{<a!>},{<a!>},{<a!>},{<ab>}}" -> 3
)

case class State(depth: Int, sum: Int) {
  def incDepth = copy(depth = depth + 1)
  def decDepth = copy(depth = depth - 1)
  def add(n: Int) = copy(sum = sum + n)
}
object State { val initial = State(0, 0) }

def score(input: String) = {
  normalize(input).foldLeft (State.initial) { (s, char) =>
    char match {
      case '{' => s.incDepth
      case '}' => s.add(s.depth).decDepth
      case _   => s
    }
  }.sum
}

examples foreach { case (input, expectedScore) =>
  val result = score(input)
  //println(Seq(expectedScore, result, input) mkString "\t")
  assert(result == expectedScore)

}

val input = scala.io.Source .fromFile("09.txt").getLines.toVector

println( input.map(score).sum )

case class CountState(garbageCount: Int, inGarbage: Boolean, escaped: Boolean) {
  def incCount     = copy(garbageCount = garbageCount + 1)
  def enterGarbage = copy(inGarbage = true)
  def exitGarbage  = copy(inGarbage = false)
  def enterEscaped = copy(escaped = true)
  def exitEscaped  = copy(escaped = false)
}
object CountState { val initial = CountState(0, false, false) }

def countGarbage(input: String) = {
  input.foldLeft (CountState.initial) { (s, char) =>
    char match {
      case _   if  s.escaped   => s.exitEscaped
      case '!'                 => s.enterEscaped
      case '<' if !s.inGarbage => s.enterGarbage
      case '>' if  s.inGarbage => s.exitGarbage
      case _   if  s.inGarbage => s.incCount
      case _                   => s
    }
  }.garbageCount
}

println(input.map(countGarbage).sum)

val countExamples = List(
   """<>"""                  -> 0
,  """<random characters>""" -> 17
,  """<<<<>"""               -> 3
,  """<{!>}>"""              -> 2
,  """<!!>"""                -> 0
,  """<!!!>>"""              -> 0
,  """<{o"i!a,<{i<a>"""   -> 10
)

countExamples.foreach { case (input, expectedCount) =>
  val count = countGarbage(input)
  //println(Seq(expectedCount, count, input) mkString "\t")
  assert(count == expectedCount)
}
