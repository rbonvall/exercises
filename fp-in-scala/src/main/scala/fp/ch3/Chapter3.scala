package fp.ch3

object Chapter3 {

  // Exercise 3.2
  def tail[T](xs: List[T]): List[T] = xs match {
    case first :: rest ⇒ rest
    case Nil ⇒ ???
  }

  // Exercise 3.3
  def setHead[T](xs: List[T], h: T): List[T] = xs match {
    case first :: rest ⇒ h :: rest
    case Nil ⇒ ???
  }

  // Exercise 3.4
  @annotation.tailrec
  def drop[T](xs: List[T], n: Int): List[T] =
    if (n == 0) xs
    else        drop(tail(xs), n - 1)

  // Exercise 3.5
  @annotation.tailrec
  def dropWhile[T](xs: List[T], f: T ⇒ Boolean): List[T] = xs match {
    case first :: rest if f(first) ⇒ dropWhile(rest, f)
    case _                         ⇒ xs
  }

  // Exercise 3.6
  def init[T](xs: List[T]): List[T] = xs match {
    case first :: last :: Nil ⇒ first :: Nil
    case first :: rest        ⇒ first :: init(rest)
    case Nil ⇒ ???
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) ⇒ B): B = as match {
    case Nil ⇒ z
    case x :: xs ⇒ f(x, foldRight(xs, z)(f))
  }
  def product(ns: List[Double]) = foldRight(ns, 1.0) { _ * _ }

  // Exercise 3.7: I don't think is possible we can get short-circuiting
  // using foldRight to define product.

  // Exercise 3.8
  def fons[A](as: List[A]) = foldRight(as, List[A]())(_ :: _)

  // Exercise 3.9
  def length[A](as: List[A]): Int = foldRight(as, 0) { (_, a) ⇒ a + 1 }

  // Exercise 3.10
  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) ⇒ B): B = as match {
    case Nil ⇒ z
    case first :: rest ⇒ foldLeft(rest, f(z, first))(f)
  }

  // Exercise 3.11
  def sumL(ns: List[Int]): Int = foldLeft(ns, 0) { _ + _ }
  def productL(xs: List[Double]): Double = foldLeft(xs, 1.0) { _ * _ }
  def lengthL[A](as: List[A]): Int = foldLeft(as, 0) { (a, _) ⇒ a + 1 }

  // Exercise 3.12
  def reverse[T](xs: List[T]): List[T] = foldLeft(xs, List[T]()) { (acc, x) ⇒ x :: acc }

  // Exercise 3.13
  def foldl[A, B](as: List[A], z: B)(f: (B, A) ⇒ B): B = foldRight(as.reverse, z) { (a, acc) ⇒ f(acc, a) }
  def foldr[A, B](as: List[A], z: B)(f: (A, B) ⇒ B): B = foldLeft (as.reverse, z) { (acc, a) ⇒ f(a, acc) }

  // Exercise 3.14
  def append[A](as: List[A], newA: A): List[A] = foldRight(as, newA :: Nil) { (a, acc) ⇒ a :: acc }

  // Exercise 3.15
  def concatenate[A](listOfLists: List[List[A]]): List[A] = foldLeft(listOfLists, List[A]()) { (bigList, oneList) ⇒
    foldLeft(oneList, bigList) (append)
  }

  // Exercise 3.16
  def mapIncrement(ints: List[Int]): List[Int] = ints match {
    case Nil     ⇒ Nil
    case n :: ns ⇒ (n + 1) :: mapIncrement(ns)
  }

  // Exercise 3.17
  def mapToString(nums: List[Double]): List[String] = nums match {
    case Nil     ⇒ Nil
    case x :: xs ⇒ (x.toString) :: mapToString(xs)
  }

  // Exercise 3.18
  def map[A, B](values: List[A]) (f: A ⇒ B): List[B] = values match {
    case Nil     ⇒ Nil
    case a :: as ⇒ f(a) :: map(as)(f)
  }

  // Exercise 3.19
  def filter[A](values: List[A]) (f: A ⇒ Boolean): List[A] = values match {
    case Nil     ⇒ Nil
    case a :: as ⇒ if (f(a)) a :: filter(as)(f)
                   else      filter(as)(f)
  }

  // Exercise 3.20
  def flatMap[A, B](values: List[A]) (f: A ⇒ List[B]): List[B] =
    concatenate(map(values)(f))

  // Exercise 3.21
  def filterFM[A](values: List[A]) (f: A ⇒ Boolean): List[A] = flatMap(values) { x ⇒
    if (f(x)) List(x) else List()
  }

  // Exercise 3.22
  def zipWithIntAdd(left: List[Int], right: List[Int]): List[Int] = (left, right) match {
    case (Nil    , _      ) ⇒ Nil
    case (_      , Nil    ) ⇒ Nil
    case (n :: ns, m :: ms) ⇒ (n + m) :: zipWithIntAdd(ns, ms)
  }

}
