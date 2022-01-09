type Input = Any

def part1(input: Input) =
  ()

def part2(input: Input) =
  ()

@main
def run =
  val src = io.Source.fromFile("11.txt")
  val input: Input =
    try src.getLines.toList
    finally src.close()

  println(part1(input))
  println(part2(input))


