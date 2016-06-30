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
}
