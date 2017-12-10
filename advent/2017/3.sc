// vim: ft=scala

import math.{sqrt, abs}

val input = 347991

// Part 1

def roundUpToOddNumber(i: Int) = i + 1 - i % 2

assert((1 to 10).map(roundUpToOddNumber) == Seq(1, 3, 3, 5, 5, 7, 7, 9, 9, 11))

def group(n: Int) = roundUpToOddNumber(sqrt(n).ceil.toInt) - 2

def numberOfStepsToCarryFrom(n: Int) = {
  val g = group(n)
  val p = (n - g * g - 1) % (g + 1)
  if (p < g / 2) g - p
  else           p + 1
}

println(numberOfStepsToCarryFrom(347991))


// Part 2

val coords = (0, 0) +: (
  for {
    g <- Stream.from(1)
    plusG  = Stream.continually(g)
    minusG = Stream.continually(-g)
    asc = (1 - g) to g
    dsc = (g - 1) to -g by -1
    dir <- Vector('up, 'left, 'down, 'right)
    xs = dir match {
      case 'up    => plusG
      case 'left  => dsc
      case 'down  => minusG
      case 'right => asc
    }
    ys = dir match {
      case 'up    => asc
      case 'left  => plusG
      case 'down  => dsc
      case 'right => minusG
    }
    pair <- xs zip ys
  } yield pair
)

val initialState = Map((0, 0) -> 1) withDefaultValue 0

val deltas = Seq(-1, 0, 1)
def neighbors(pair: (Int, Int)) = {
  val (x, y) = pair
  for {
    dx <- deltas
    dy <- deltas
    if dx != 0 || dy != 0
  } yield (x + dx, y + dy)
}

coords.tail
  .scanLeft(initialState) { (state, c) =>
    state.updated(c, neighbors(c).map(state).sum)
  }
  .map(_.values.max)
  .find(_ > input)
  .foreach(println)

//coords zip Stream.from(1) take 26 foreach { case ((x, y), n) =>
//  println(s"$n\t($x,\t$y)")
//}
