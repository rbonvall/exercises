def part1(initialTimers: List[Int]) =
  Iterator
    .iterate(initialTimers) { timers =>
      timers.flatMap {
        case 0 => List(6, 8)
        case t => List(t - 1)
      }
    }
    .drop(80)
    .next
    .length

def part2(timers: List[Int]) =
  ()

@main
def run =
  val src = io.Source.fromFile("06.txt")
  val input: List[Int] =
    try src.mkString.trim.split(",").toList.map(_.toInt)
    finally src.close()

  println(part1(input))
  println(part2(input))

