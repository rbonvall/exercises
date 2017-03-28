package fp.ch4

object Chapter4 {

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else            Some(xs.sum / xs.length)

  def sq(x: Double) = x * x

  // Exercise 4.2
  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).filter(_ ⇒ xs.length > 1).map { μ ⇒
      xs.map(μ - _).map(sq).sum / (xs.length - 1)
    }

}
