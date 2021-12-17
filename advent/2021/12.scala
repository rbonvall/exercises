def traversePaths(caves: List[(String, String)], current: String, visited: Set[String]): List[List[String]] =
  if current == "end"
  then List(List(current))
  else
    val newVisited =
      if current.charAt(0).isLower
      then visited + current
      else visited
    caves
      .collect {
        case (`current`, dst) if !visited.contains(dst) => dst
        case (dst, `current`) if !visited.contains(dst) => dst
      }
      .flatMap(dst => traversePaths(caves, dst, newVisited))
      .map(path => current :: path)

def part1(caves: List[(String, String)]) =
  traversePaths(caves, "start", Set.empty).length

def part2(caves: List[(String, String)]) =
  ()

@main
def run =
  val src = io.Source.fromFile("12.txt")
  val input: List[(String, String)] =
    try src.getLines
      .filter(_.nonEmpty)
      .map { line =>
        val i = line.indexOf('-')
        (line.take(i), line.drop(i + 1))
      }
      .toList
    finally src.close()

  println(part1(input))
  println(part2(input))

