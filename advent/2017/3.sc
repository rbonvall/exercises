// vim: ft=scala

import math.{sqrt, abs}

val input = 347991

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

