package fp.ch4

import org.scalatest.FunSpec
import org.scalactic.TolerantNumerics

class Chapter4Spec extends FunSpec {
  import Chapter4._

  describe("variance") {
    it("safely computes the variance of a sequence of numbers") {
      assert(variance(Nil) === None)
      assert(variance(Seq(3)) === None)
      assert(variance(Seq(3, 5)) === Some(2))
      assert(variance(Seq(3, 3, 3)) === Some(0))

    }
  }

}
