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

}
