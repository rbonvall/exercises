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


}
