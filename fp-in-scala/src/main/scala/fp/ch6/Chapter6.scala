package fp.ch6

object Chapter6 {

  object FirstAPI {

    def randomPair(rng: RNG): ((Int, Int), RNG) = {
      val (i1, rng1) = rng.nextInt
      val (i2, rng2) = rng1.nextInt
      ((i1, i2), rng2)
    }

    def randomDouble(rng: RNG): (Double, RNG) = {
      val (i, rng1) = rng.nextInt
      val x = ???
      (x, rng1)
    }

    def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
      if (count == 0) (Nil, rng)
      else {
        val (i, rng1) = rng.nextInt
        val (rest, rng2) = ints(count - 1)(rng1)
        (i :: rest, rng2)
      }
    }

  }

  type Rand[+A] = RNG => (A, RNG)

  object BetterAPI {

    val int: Rand[Int] = _.nextInt

    def unit[A](a: A): Rand[A] =
      rng => (a, rng)

    def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
      rng => {
        val (a, rng1) = s(rng)
        (f(a), rng1)
      }

    // Exercise 6.6, part 1
    def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
      rng => {
        val (a, rng1) = ra(rng)
        val (b, rng2) = rb(rng1)
        (f(a, b), rng2)
      }

    // Exercise 6.6, part 2
    def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
      map2(ra, rb)((_, _))

    // Exercise 6.7, part 1
    def sequence[A](rs: List[Rand[A]]): Rand[List[A]] =
      rs match {
        case Nil       => unit(Nil)
        case r :: rest => map2(r, sequence(rest))(_ :: _)
      }

    // Exercise 6.7, part 2
    def intsSeq(count: Int): Rand[List[Int]] =
      sequence(List.fill(count)(int))

  }

}
