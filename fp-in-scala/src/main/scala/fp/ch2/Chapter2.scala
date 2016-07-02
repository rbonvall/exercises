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

}
