package tapl.arith

import org.scalatest.FunSpec

class TermSpec extends FunSpec {

  describe("S") {
    it("generates terms in the arith language") {
      import Term._

      assert(S(0).isEmpty)
      assert(S(1) === Set(True, False, Zero))
      assert(S(2) === Set(
        True
      , False
      , Zero
      , Succ(True ), Pred(True ), IsZero(True )
      , Succ(False), Pred(False), IsZero(False)
      , Succ(Zero ), Pred(Zero ), IsZero(Zero )
      , Cond(True, True,  True ), Cond(False, True,  True ), Cond(Zero, True,  True )
      , Cond(True, True,  False), Cond(False, True,  False), Cond(Zero, True,  False)
      , Cond(True, True,  Zero ), Cond(False, True,  Zero ), Cond(Zero, True,  Zero )
      , Cond(True, False, True ), Cond(False, False, True ), Cond(Zero, False, True )
      , Cond(True, False, False), Cond(False, False, False), Cond(Zero, False, False)
      , Cond(True, False, Zero ), Cond(False, False, Zero ), Cond(Zero, False, Zero )
      , Cond(True, Zero,  True ), Cond(False, Zero,  True ), Cond(Zero, Zero,  True )
      , Cond(True, Zero,  False), Cond(False, Zero,  False), Cond(Zero, Zero,  False)
      , Cond(True, Zero,  Zero ), Cond(False, Zero,  Zero ), Cond(Zero, Zero,  Zero )
      ))

      // Exercise 3.2.4: How many elements does S₃ have?
      val n = S(2).size
      assert(S(3).size === 3 + 3 * n + n * n * n)

    }
  }

  describe("eval") {
    import Term._
    implicit class EvalsTo(term: Term) {
      def ↦(result: Term) = Term.eval(term) === result
    }

    it("returns constants unchanged") {
      assert(True  ↦ True)
      assert(False ↦ False)
      assert(Zero  ↦ Zero)
    }

    it("evaluates a numerical term") {
      assert(Succ(Zero)             ↦ Succ(Zero))
      assert(Pred(Succ(Zero))       ↦ Zero)
      assert(Succ(Pred(Succ(Zero))) ↦ Succ(Zero))
      assert(Pred(Zero)             ↦ Zero)
    }

    it("evaluates conditionals") {
      assert(Cond(True,  Zero, Succ(Zero)) ↦ Zero)
      assert(Cond(False, Zero, Succ(Zero)) ↦ Succ(Zero))
    }

    it("evaluates nested expressions") {
      val t1 = Cond(True, Cond(False, False, False), True)
      assert(t1 ↦ False)

      val t2 = Pred(Cond(Cond(True, False, True), Succ(Zero), Succ(Succ(Zero))))
      assert(t2 ↦ Succ(Zero))
    }

    it("returns nonsensical expressions unchanged") {
      assert(Pred(False) ↦ Pred(False))
      assert(Cond(Zero, True, False) ↦ Cond(Zero, True, False))
    }

  }

}
