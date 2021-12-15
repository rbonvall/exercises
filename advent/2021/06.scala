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

def stepTimerCount(timerCount: Map[Int, Long]): Map[Int, Long] =
  Map(
    0 -> timerCount(1),
    1 -> timerCount(2),
    2 -> timerCount(3),
    3 -> timerCount(4),
    4 -> timerCount(5),
    5 -> timerCount(6),
    6 -> (timerCount(7) + timerCount(0)),
    7 -> timerCount(8),
    8 -> timerCount(0),
  )

def part2(initialTimers: List[Int]) =
  val initialTimerCount = initialTimers
    .groupBy(identity)
    .map { (k, v) => (k, v.length.toLong) }
    .withDefaultValue(0L)
  Iterator
    .iterate(initialTimerCount)(stepTimerCount)
    .drop(256)
    .next
    .values
    .sum

@main
def run =
  val src = io.Source.fromFile("06.txt")
  val input: List[Int] =
    try src.mkString.trim.split(",").toList.map(_.toInt)
    finally src.close()

  println(part1(input))
  println(part2(input))

