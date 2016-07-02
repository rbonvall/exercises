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
}
