package fp.ch4

import org.scalatest.FunSpec

class OptionSpec extends FunSpec {

  describe("map") {
    it("maps") {
      val aNum = Some(3)
      def f(n: Int): String = "x" * n
      assert(aNum.map(f) === Some("xxx"))
      assert(None.map(f) === None)
    }
  }

  describe("orElse") {
    it("orElses") {
      assert((Some(5) orElse Some(6)) === Some(5))
      assert((Some(5) orElse None   ) === Some(5))
      assert((None    orElse Some(6)) === Some(6))
      assert((None    orElse None   ) === None)

    }
  }

  case class Fraction(num: Int, den: Int)
  implicit class IntToFrac(n: Int) {
    def ÷(d: Int) = new Fraction(n, d)
  }

  def invert(q: Fraction): Option[Fraction] =
    if (q.num == 0) None
    else            Some(Fraction(q.den, q.num))

  describe("flatMapPM") {
    it("flatMaps using pattern matching") {
      assert(Some(3 ÷ 2).flatMapPM(invert) === Some(2 ÷ 3))
      assert(Some(0 ÷ 3).flatMapPM(invert) === None)
      assert(None       .flatMapPM(invert) === None)
    }
  }

  describe("flatMap") {
    it("flatMaps without using pattern matching") {
      assert(Some(3 ÷ 2).flatMap(invert) === Some(2 ÷ 3))
      assert(Some(0 ÷ 3).flatMap(invert) === None)
      assert(None       .flatMap(invert) === None)
    }
  }

  describe("getOrElsePM") {
    it("gets the value or else the default value using pattern matching") {
      assert(Some(5).getOrElsePM(6) === 5)
      assert(None   .getOrElsePM(6) === 6)
    }
  }

  describe("getOrElse") {
    it("gets the value or else the default value without using pattern matching") {
      assert(Some(5).getOrElse(6) === 5)
      assert(None   .getOrElse(6) === 6)
    }
  }

  def even(n: Int): Boolean = n % 2 == 0

  describe("filterPM") {
    it("filters à la patmat") {
      assert(Some(6).filterPM(even) === Some(6))
      assert(Some(5).filterPM(even) === None)
      assert(None   .filterPM(even) === None)
    }
  }

}
