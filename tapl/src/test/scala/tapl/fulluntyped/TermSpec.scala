package tapl.fulluntyped

import org.scalatest.FunSpec

class TermSpec extends FunSpec {
  import DeBruĳn._
  import Term.{λ, symToVar}
  import NamelessTerm.{λĳ, intToIndex}

  describe("λ") {
    it("creates abstractions") {
      val body = Var('x)
      val nice = λ('x) { body }
      val ugly = Abs(Var('x), body)
      assert(nice === ugly)
    }
    it("creates curried abstractions when given several parameters") {
      val body = App(Var('x), Var('z))
      val nice = λ('x, 'y, 'z) { body }
      val ugly = Abs(Var('x), Abs(Var('y), Abs(Var('z), body)))
      assert(nice === ugly)
    }
  }

  describe("λĳ") {
    it("creates nameless abstractions") {
      val body = NApp(Index(1), NApp(Index(0), Index(1)))
      val nice = λĳ { body }
      val ugly = NAbs(body)
      assert(nice === ugly)
    }
  }

  describe("removeNames") {
    it("converts an ordinaty term into its nameless representation") {
      val nameful  = λ('x, 'y) { 'x $ ('x $ 'y) } $ λ('x) { 'a $ 'x }
      val nameless = λĳ { λĳ { 1 $ (1 $ 0)  }} $ λĳ { 1 $ 0 }
      val result = removeNames(List('a), nameful)
      assert(result === nameless)
    }
  }

  describe("restoreNames") {
    it("converts a nameless term into a ordinary term with fresh variables") {
      val nameless = λĳ { λĳ { 1 $ (1 $ 0)  }} $ λĳ { 1 $ 0 }
      val nameful  = λ('x, 'y) { 'x $ ('x $ 'y) } $ λ('x) { 'a $ 'x }
      val result = restoreNames(List('a), nameless)
      assert(result === nameful)
    }
  }

  describe("newVariable") {
    it("returns a variable name that's not present in the given naming context") {
      assert(newVariable(Nil) === 'x)
      assert(newVariable(List('a)) === 'x)
      assert(newVariable(List('x)) === 'y)
      assert(newVariable(List('b, 'a, 'x, 'z, 'y)) === 'c)
    }
    it("returns uppercase names when it runs out of lowercase names") {
      val all = ('a' to 'z').map { c ⇒ Symbol(c.toString) }.toList
      assert(newVariable(all) === 'X)
      assert(newVariable('X :: all) === 'Y)
    }
    it("returns indexed variables when it runs out of letters") {
      val all = (('a' to 'z') ++ ('A' to 'Z')).map { c ⇒ Symbol(c.toString) }.toList
      assert(newVariable(all) === 'x1)
      assert(newVariable('x1 :: all) === 'x2)
    }
  }

  describe("shift") {
    ignore("works for exercise 6.2.2-1") {
      val term = λĳ { λĳ { 1 $ (0 $ 2)}}
      val result = shift(2)(term)
      val expected = ???
      assert(result === expected)
    }
    ignore("works for exercise 6.2.2-2") {
      val term = λĳ { 0 $ 1 $ λĳ { 0 $ 1 $ 2 }}
      val result = shift(2)(term)
      val expected = ???
      assert(result === expected)
    }
  }

}



