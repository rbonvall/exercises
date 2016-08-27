package fp.ch3

import org.scalatest.FunSpec
import org.scalactic.TolerantNumerics

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
    it("drops as many elements as asked") {
      assert(drop(xs, 0) === xs)
      assert(drop(xs, 1) === List("b", "c", "d"))
      assert(drop(xs, 2) === List("c", "d"))
      assert(drop(xs, 3) === List("d"))
    }
  }

  describe("dropWhile") {
    it("drops elements while the predicate is true") {
      assert(dropWhile(xs, { x: String ⇒ x != "c" }) === List("c", "d"))
      assert(dropWhile(xs, { x: String ⇒ false }) === xs)
      assert(dropWhile(xs, { x: String ⇒ true  }) === Nil)
      assert(dropWhile(List[Int](), { x: Int ⇒ x > 2 }) === Nil)
    }
  }

  describe("init") {
    it("gives all elements but the last one") {
      assert(init(xs) === List("a", "b", "c"))
    }
  }

  describe("folding with cons") {
    it("keeps the list unchanged") {
      val ns = List(1, 2, 3)
      assert(fons(ns) === ns)

    }
  }

  describe("length") {
    it("computes the length of a list") {
      assert(length(Nil) === 0)
      assert(length(xs) === 4)
    }
  }

  describe("foldLeft") {
    it("folds") {
      assert(foldLeft(xs, "")(_ ++ _) === "abcd")
    }
  }

  describe("sumL") {
    it("adds integers by folding from the left") {
      assert(sumL(Nil) === 0)
      assert(sumL(List(66, 11, 22)) === 99)
    }
  }

  describe("productL") {
    it("multiplies elements by folding from the left") {
      implicit val doubleEq = TolerantNumerics.tolerantDoubleEquality(1e-2)
      assert(productL(Nil) === 1.0)
      assert(productL(List(10.0, 1.0, 3.14)) === 31.4)
    }
  }

  describe("lengthL") {
    it("counts stuff by folding from the left") {
      assert(lengthL(Nil) === 0)
      assert(lengthL(xs) === 4)
    }
  }



}
