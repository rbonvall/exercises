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

  describe("filterPM") {
    it("filters, no patmat, flatMappin' that shit") {
      assert(Some(6).filter(even) === Some(6))
      assert(Some(5).filter(even) === None)
      assert(None   .filter(even) === None)
    }
  }

  describe("lift") {
    it("lifts") {
      assert(Option.lift(even)(Some(-8)) === Some(true))
      assert(Option.lift(even)(Some(15)) === Some(false))
      assert(Option.lift(even)(None)     === None)
    }
  }

  describe("Try") {
    it("tries very hard") {
      assert(Option.Try(1 / 0) === None)
      assert(Option.Try(4 / 2) === Some(2))
    }
  }

  def rep(n: Int, s: String): List[String] = List.fill(n)(s)

  describe("map2") {
    it("returns some result iff both arguments are defined") {
      assert(Option.map2(None,    None     )(rep) === None)
      assert(Option.map2(Some(3), None     )(rep) === None)
      assert(Option.map2(None,    Some("x"))(rep) === None)
      assert(Option.map2(Some(3), Some("x"))(rep) === Some(List("x", "x", "x")))
    }
  }

  describe("sequence") {
    it("combines a list of options into one option") {
      assert(Option.sequence(List(Some(1), Some(2), Some(3))) === Some(List(1, 2, 3)))
      assert(Option.sequence(List(Some(1), None,    Some(3))) === Some(None))
      assert(Option.sequence(Nil) === Some(Nil))
    }
  }

}
