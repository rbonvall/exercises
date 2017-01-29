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

}
