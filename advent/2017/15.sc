// vim: ft=scala

def create(factor: Long)(last: Long) =
  (last * factor) % 2147483647L

def lowBits(n: Long) = n & 0xFFFFL

object A extends Iterable[Long] {
  def iterator = Iterator.iterate(591L)(create(16807L))
}
object B extends Iterable[Long] {
  def iterator = Iterator.iterate(393L)(create(48271L))
}

def tupleEq[A](pair: (A, A)) = pair._1 == pair._2

def compareGenerators(howMany: Int)(a: Iterable[Long], b: Iterable[Long]): Int = {
  (a.iterator map lowBits) zip (b.iterator map lowBits) take howMany count tupleEq
}

object PickyA extends Iterable[Long] {
  def iterator = A.iterator.filter(_ % 4 == 0)
}
object PickyB extends Iterable[Long] {
  def iterator = B.iterator.filter(_ % 8 == 0)
}

println(compareGenerators( 5000000)(PickyA, PickyB))
println(compareGenerators(40000000)(A, B))

//val matchCount =
//  (1 to 40000000).foldLeft((0, 591L, 393L)) { case (state, i) =>
//    val (count, lastA, lastB) = state
//    val newA = create(16807L)(lastA)
//    val newB = create(48271L)(lastB)
//    val Δc = if (lowBits(newA) == lowBits(newB)) 1 else 0
//    (count + Δc, newA, newB)
//  }



