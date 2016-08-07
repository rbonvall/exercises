package tapl.arith

sealed trait Term
object Term {
  case object True            extends Term
  case object False           extends Term
  case object Zero            extends Term
  case class  Succ  (t: Term) extends Term
  case class  Pred  (t: Term) extends Term
  case class  IsZero(t: Term) extends Term
  case class  Cond( `if`: Term
                  , then: Term
                  , `else`: Term) extends Term

  def S(i: Int): Set[Term] =
    if (i == 0) Set.empty
    else {
      val s = S(i - 1)
      Set(True, False, Zero) ++
      (for { t1 ← s } yield Succ(t1)) ++
      (for { t1 ← s } yield Pred(t1)) ++
      (for { t1 ← s } yield IsZero(t1)) ++
      (for { t1 ← s ; t2 ← s ; t3 ← s } yield Cond(t1, t2, t3))
    }

  def terms = Stream.from(0).flatMap { i ⇒ S(i) }

}

