package fp.ch2

object Chapter2 {

  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(i: Int, prev: Int, curr: Int): Int = {
      if      (i == 0) prev
      else if (i == 1) curr
      else             go(i - 1, curr, prev + curr)
    }
    go(n, 0, 1)
  }

  // @annotation.tailrec
  // def isSorted[A](as: Array[A], ordered: (A, A) ⇒ Boolean): Boolean = {
  //   if (as.length < 2)               true
  //   else if (!ordered(as(0), as(1))) false
  //   else                             isSorted(as.tail, ordered)
  // }

  @annotation.tailrec
  def isSorted[A](as: Array[A], ordered: (A, A) ⇒ Boolean): Boolean =
    as.length < 2 || ordered(as(0), as(1)) && isSorted(as.tail, ordered)

  def partial1[A, B, C](a: A, f: (A, B) ⇒ C): B ⇒ C =
    (b: B) ⇒ f(a, b)

  // ⇒ associates to the right,
  // so A ⇒ B ⇒ C is the same as A ⇒ (B ⇒ C).

  def curry[A, B, C](f: (A, B) ⇒ C): A ⇒ B ⇒ C =
    a ⇒ b ⇒ f(a, b)

  def uncurry[A, B, C](f: A ⇒ B ⇒ C): (A, B) ⇒ C =
    (a, b) ⇒ f(a)(b)

  def compose[A, B, C](f: B ⇒ C, g: A ⇒ B): A ⇒ C =
    a ⇒ f(g(a))

}
