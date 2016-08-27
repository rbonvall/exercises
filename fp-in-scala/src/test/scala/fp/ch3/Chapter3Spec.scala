package fp.ch3

import org.scalatest.FunSpec

class Chapter3Spec extends FunSpec {
  import Chapter3._

  val xs = List("a", "b", "c", "d")

  describe("tail") {
    it("tails") {
      assert(tail(xs) === List("b", "c", "d"))
    }
  }

  describe("setHead") {
    it("sets head") {
      assert(setHead(xs, "z") === List("z", "b", "c", "d"))
    }
  }

  describe("drop") {
    it("drops") {
      assert(drop(xs, 0) === xs)
      assert(drop(xs, 1) === List("b", "c", "d"))
      assert(drop(xs, 2) === List("c", "d"))
      assert(drop(xs, 3) === List("d"))
    }
  }


}
