package tapl.arith

import org.scalatest.FunSpec

class TermSpec extends FunSpec {

  describe("S") {
    it("generates terms in the arith language") {
      import Term._

      assert(S(0).isEmpty)
      assert(S(1) === Set(True, False, Zero))
      assert(S(2) === Set(
        True,
        False,
        Zero,
        Succ(True),  Pred(True),  IsZero(True),
        Succ(False), Pred(False), IsZero(False),
        Succ(Zero),  Pred(Zero),  IsZero(Zero),
        Cond(True, True,  True ), Cond(False, True,  True ), Cond(Zero, True,  True ),
        Cond(True, True,  False), Cond(False, True,  False), Cond(Zero, True,  False),
        Cond(True, True,  Zero ), Cond(False, True,  Zero ), Cond(Zero, True,  Zero ),
        Cond(True, False, True ), Cond(False, False, True ), Cond(Zero, False, True ),
        Cond(True, False, False), Cond(False, False, False), Cond(Zero, False, False),
        Cond(True, False, Zero ), Cond(False, False, Zero ), Cond(Zero, False, Zero ),
        Cond(True, Zero,  True ), Cond(False, Zero,  True ), Cond(Zero, Zero,  True ),
        Cond(True, Zero,  False), Cond(False, Zero,  False), Cond(Zero, Zero,  False),
        Cond(True, Zero,  Zero ), Cond(False, Zero,  Zero ), Cond(Zero, Zero,  Zero )
      ))

    }
  }

}
