def part1(initialPositions: List[Int]) =
  (0 to initialPositions.max)
    .map { t => 
      initialPositions.map { n => (t - n).abs }.sum
    }
    .min

def part2(initialPositions: List[Int]) =
  ()

@main
def run =
  val src = io.Source.fromFile("07.txt")
  val input: List[Int] =
    try src.mkString.trim.split(",").toList.map(_.toInt)
    finally src.close()

  println(part1(input))
  println(part2(input))

