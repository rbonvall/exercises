// vim: ft=scala

case class Component(a: Int, b: Int) {
  def reversed = Component(b, a)
  def sorted = Component(a min b, a max b)
  def strength = a + b
  def contains(n: Int) = a == n || b == n
  def isZeroPin = contains(0)
  def canBeConnectedWith(that: Component) = that.contains(a) || that.contains(b)
  def connectingTo(n: Int) = if (b == n) reversed else this
  override def toString = s"$a/$b"
}
object Component {
  def fromTuple(pair: (Int, Int)) = Component(pair._1, pair._2)
}

val example = Seq(
  (0, 2), (2, 2), (2, 3), (3, 4), (3, 5), (0, 1), (10, 1), (9, 10)
).map(Component.fromTuple)

val input =  Seq(
  (42, 37), (28, 28), (29, 25), (45,  8), (35, 23), (49, 20),
  (44,  4), (15, 33), (14, 19), (31, 44), (39, 14), (25, 17),
  (34, 34), (38, 42), ( 8, 42), (15, 28), ( 0,  7), (49, 12),
  (18, 36), (45, 45), (28,  7), (30, 43), (23, 41), ( 0, 35),
  (18,  9), ( 3, 31), (20, 31), (10, 40), ( 0, 22), ( 1, 23),
  (20, 47), (38, 36), (15,  8), (34, 32), (30, 30), (30, 44),
  (19, 28), (46, 15), (34, 50), (40, 20), (27, 39), ( 3, 14),
  (43, 45), (50, 42), ( 1, 33), ( 6, 39), (46, 44), (22, 35),
  (15, 20), (43, 31), (23, 23), (19, 27), (47, 15), (43, 43),
  (25, 36), (26, 38), ( 1, 10)
).map(Component.fromTuple)

// There are no repeated components
assert(example.toSet.size == example.size)
assert(input  .toSet.size == input  .size)

def bridgesStartingWith(prefix: Seq[Component], remaining: Set[Component]): Seq[Seq[Component]] = {
  val last = prefix.last
  val nextComponents = remaining.filter(_ contains last.b).toSeq
  val nextBridges = nextComponents.flatMap { next ⇒
    bridgesStartingWith(prefix :+ next.connectingTo(last.b), remaining - next)
  }
  prefix +: nextBridges
}

def allBridges(cs: Seq[Component]): Seq[Seq[Component]] =
  bridgesStartingWith(Seq(Component(0, 0)), cs.toSet)
    .map(_.tail)
    .filter(_.nonEmpty)

//allBridges(example).map(_.mkString("--")).foreach(println)

// Part 1
def maxStrength(bridges: Seq[Seq[Component]]) =
  bridges.map(_.map(_.strength).sum).max

// Part 2
def longestStrongestBridgeStrength(bridges: Seq[Seq[Component]]) =
  bridges.map { b ⇒ (b.length, b.map(_.strength).sum) }.max._2

val exampleBridges = allBridges(example)
println(maxStrength(exampleBridges))
println(longestStrongestBridgeStrength(exampleBridges))

val inputBridges = allBridges(input)
println(maxStrength(inputBridges))
println(longestStrongestBridgeStrength(inputBridges))
