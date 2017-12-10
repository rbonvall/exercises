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

val input = scala.io.Source .fromFile("9.txt").getLines.toVector

println( input.map(score).sum )

