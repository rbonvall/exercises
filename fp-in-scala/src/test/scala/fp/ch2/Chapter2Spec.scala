package fp.ch2

import org.scalatest.FunSpec

class Chapter2Spec extends FunSpec {
  import Chapter2._

  describe("fib") {
    it("gets the nth Fibonacci number") {
      assert(fib(0) ===  0)
      assert(fib(1) ===  1)
      assert(fib(2) ===  1)
      assert(fib(3) ===  2)
      assert(fib(4) ===  3)
      assert(fib(5) ===  5)
      assert(fib(6) ===  8)
      assert(fib(7) === 13)
      assert(fib(8) === 21)
      assert(fib(9) === 34)
    }
  }

  describe("isSorted") {
    it("checks whether an Array[A] is sorted according to a given comparison function") {
      assert( isSorted[Int](Array(1, 2, 3, 4, 5), _ <= _))
      assert(!isSorted[Int](Array(1, 2, 3, 4, 5), _ >= _))
      assert( isSorted[Int](Array(6, 2, 4, 5, 3, 1), _ % 2 <= _ % 2))
      assert(!isSorted[Int](Array(1, 2, 3, 4, 5, 6), _ % 2 <= _ % 2))
      assert( isSorted[String](Array("black", "blue", "red", "yellow"), _ <= _))
      assert(!isSorted[String](Array("black", "blue", "red", "yellow"), _.length <= _.length))
      assert( isSorted[String](Array("red", "blue", "black", "yellow"), _.length <= _.length))

    }
  }

  describe("partial1") {
    it("partially applies a binary function with the first parameter") {
      val f = partial1(0, Math.max)
      assert(f(-2) === 0)
      assert(f(-1) === 0)
      assert(f( 0) === 0)
      assert(f( 1) === 1)
      assert(f( 2) === 2)
    }
  }

  describe("curry") {
    it("curries") {
      val f = curry((a: Int, b: Int) => a * b)
      val neg = f(-1)
      assert(neg(15) === -15)
      assert(neg(-8) === 8)
    }
  }

  describe("uncurry") {
    it("uncurries") {
      val mul = uncurry((a: Int) ⇒ (b: Int) ⇒ a * b)
      assert(mul(3, 4) === 12)
    }
  }

  describe("compose") {
    it("composes") {
      def next(n: Int) = n + 1
      def twice(n: Int) = 2 * n
      val nt = compose(next, twice)
      val tn = compose(twice, next)
      assert(nt(5) === 11)
      assert(tn(5) === 12)
    }
  }

}
