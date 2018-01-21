// vim: ft=scala

import math.abs

case class Point(x: Int, y: Int, z: Int) {
  def distToOrigin = abs(x) + abs(y) + abs(z)
}

case class Particle(p: Point, v: Point, a: Point) {
  def next = {
    val newV = v + a
    val newP = p + newV
    Particle(newP, newV, a)
  }
}

object ParticleDescription {
  private val threeNums = Seq.fill(3)("""(-?\d+)""")
  private val pattern = List(
    threeNums.mkString("p=<", ",", ">"),
    threeNums.mkString("v=<", ",", ">"),
    threeNums.mkString("a=<", ",", ">")
  ).mkString(", ").r
  def unapply(line: String): Option[(Point, Point, Point)] = line match {
    case pattern(p1, p2, p3, v1, v2, v3, a1, a2, a3) ⇒
      val p = Point(p1.toInt, p2.toInt, p3.toInt)
      val v = Point(v1.toInt, v2.toInt, v3.toInt)
      val a = Point(a1.toInt, a2.toInt, a3.toInt)
      Some((p, v, a))
    case _ ⇒ None
  }
}

val input: Vector[Particle] = {
  val src = io.Source.fromFile("20.txt")
  src.getLines.toVector.map {
    case ParticleDescription(p, v, a) => Particle(p, v, a)
    case _ => ???
  }
}

def iteratorWithRunLength[T](it: Iterator[T]): Iterator[(T, Int)] = {
  if (it.hasNext)
    it.scanLeft ((it.next, 1)) { case ((last, count), t) ⇒
      if (t == last) (t, 1 + count)
      else           (t, 1)
    }
  else Iterator.empty
}

val testIterator = Iterator(55, 55, 33, 33, 33, 11, 99, 99, 99, 99)
assert(
  iteratorWithRunLength(testIterator).toList == List(
    (55, 1), (55, 2),
    (33, 1), (33, 2), (33, 3),
    (11, 1),
    (99, 1), (99, 2), (99, 3), (99, 4)
  )
)

// Part 1
val states: Iterator[Seq[(Particle, Int)]] =
  Iterator.iterate(input.zipWithIndex) { particlesWithIndex ⇒
    particlesWithIndex.map { case (particle, i) ⇒ (particle.next, i) }
  }

val closestParticle: Iterator[Int] = states.map { particlesWithIndex ⇒
  val (particle, index) = particlesWithIndex.minBy(_._1.p.distToOrigin)
  index
}

// I'll assume a run length of 1000 indicates convergence.
println(
  iteratorWithRunLength(closestParticle)
    .collect { case (n, 1000) ⇒ n }
    .next
)


