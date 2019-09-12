package fp.ch6

import org.scalatest.FunSpec
import Chapter6._

class Chapter6Spec extends FunSpec {
  val rng = DumbRNG(0)

  describe("FirstAPI") {
    import Chapter6.FirstAPI._

    describe("randomPair") {
      it("generates a random pair") {
        val (pair1, rng1) = randomPair(rng)
        val (pair2, rng2) = randomPair(rng1)
        assert(pair1 === (11, 22))
        assert(pair2 === (33, 44))
      }
    }

    describe("randomDouble") {
      ignore("generates a random double") {

      }
    }

    describe("ints") {
      it("generates a list of ints") {
        val (ls1, rng1) = ints(3)(rng)
        val (ls2, rng2) = ints(5)(rng1)
        assert(ls1 === List(11, 22, 33))
        assert(ls2 === List(44, 55, 66, 77, 88))
      }
    }
  }

  describe("BetterAPI") {
    import BetterAPI._

    describe("unit") {
      it("creates a constant random number") {
        val rand = unit(123)
        val (a, rng1) = rand(rng)
        val (b, rng2) = rand(rng1)
        assert(a === 123)
        assert(b === 123)
      }
    }

    describe("map") {
      it("maps") {
        val adding = map(FirstAPI.randomPair) { pair => pair._1 + pair._2 }
        val (n1, rng1) = adding(rng)
        val (n2, rng2) = adding(rng1)
        assert(n1 === 11 + 22)
        assert(n2 === 33 + 44)
      }
    }

    describe("map2") {
      it("combines two random values into one") {
        val appending = map2(int, FirstAPI.ints(3)){ (i, is) => is :+ (i * 10 + i % 10) }
        val (ns1, rng1) = appending(rng)
        val (ns2, rng2) = appending(rng1)
        assert(ns1 === List(22, 33, 44, 111))
        assert(ns2 === List(66, 77, 88, 555))

      }
    }

    describe("both") {
      it("zips two random values") {
        // letters will be generated in this order: k v g r c n y j u
        val letters: Rand[Char] = map(int) { i => ('a' - 1 + i % 26).toChar }
        val pairs: Rand[(Int, Int)] = FirstAPI.randomPair
        val lettersAndPairs = both(letters, pairs)
        val (t1, rng1) = lettersAndPairs(rng)
        val (t2, rng2) = lettersAndPairs(rng1)
        val (t3, rng3) = lettersAndPairs(rng2)
        assert(t1 === ('k', (22, 33)))
        assert(t2 === ('r', (55, 66)))
        assert(t3 === ('y', (88, 99)))
      }
    }

    describe("sequence") {
      it("turns a list of random values inside out") {
        val threeInts: List[Rand[Int]] = List(
          int,
          map(int)(_ * 10),
          map(int)(_ / 11)
        )
        val insideOut: Rand[List[Int]] = sequence(threeInts)
        val (ns1, rng1) = insideOut(rng)
        val (ns2, rng2) = insideOut(rng1)
        val (ns3, rng3) = insideOut(rng2)
        assert(ns1 === List(11, 220, 3))
        assert(ns2 === List(44, 550, 6))
        assert(ns3 === List(77, 880, 9))
      }
    }

    describe("intsSeq") {
      it("generates a list of ints, implemented using sequence") {
        val (ls1, rng1) = intsSeq(3)(rng)
        val (ls2, rng2) = intsSeq(5)(rng1)
        assert(ls1 === List(11, 22, 33))
        assert(ls2 === List(44, 55, 66, 77, 88))
      }
    }

  }


}
