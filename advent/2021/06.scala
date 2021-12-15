def stepTimer(timer: Int): List[Int] =
  if timer == 0 then List(6, 8)
  else               List(timer - 1)

def part1(initialTimers: List[Int]) =
  LazyList.iterate(initialTimers)(_.flatMap(stepTimer))(80).length

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

