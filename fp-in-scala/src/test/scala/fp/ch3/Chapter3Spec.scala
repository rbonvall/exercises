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

  describe("reverse") {
    it("reverses a list") {
      assert(reverse(Nil) === Nil)
      assert(reverse(xs) === List("d", "c", "b", "a"))
    }
  }

  describe("foldl") {
    it("implements foldLeft using foldRight") {
      assert(foldl   ("12345".toList, 0) { (acc, a) ⇒ acc * 10 + a.toString.toInt } === 12345)
      assert(foldLeft("12345".toList, 0) { (acc, a) ⇒ acc * 10 + a.toString.toInt } === 12345)
    }
  }

  describe("foldr") {
    it("implements foldRight using foldLeft") {
      assert(foldr    ("12345".toList, 0) { (a, acc) ⇒ acc * 10 + a.toString.toInt } === 54321)
      assert(foldRight("12345".toList, 0) { (a, acc) ⇒ acc * 10 + a.toString.toInt } === 54321)
    }
  }

  describe("append") {
    it("implements append in terms of foldRight") {
      assert(append(1 :: 2 :: 3 :: Nil, 4) === 1 :: 2 :: 3 :: 4 :: Nil)
    }
  }

  describe("concatenate") {
    it("concatenates a list of lists into a single list in linear time") {
      val listOfLists = List(List(1, 2, 3), List(4, 5), List(), List(6, 7, 8, 9))
      assert(concatenate(listOfLists) === List(1, 2, 3, 4, 5, 6, 7, 8, 9))
    }
  }

  describe("mapIncrement") {
    it("transforms a list of integers by adding 1 to each element") {
      assert(mapIncrement(List(6, 1, -1, 4)) === List(7, 2, 0, 5))
    }
  }

  describe("mapToString") {
    it("turns each value in a list of numbers into a string") {
      assert(mapToString(List(1.1, -2.2, 3.14159)) === List("1.1", "-2.2", "3.14159"))
    }
  }

  describe("map") {
    it("modifies each element in a list while maintaining its structure") {
      assert(map(List(6, 1, -1, 4)) { n ⇒ (n + 1).toString } === List("7", "2", "0", "5"))
    }
  }

}
