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
                  , `then`: Term
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

  val terms: Stream[Term] = Stream.from(0).flatMap(S).distinct

  def consts(t: Term): Set[Term] = t match {
    case c @ (True | False | Zero) ⇒ Set(c)
    case Succ(t)       ⇒ consts(t)
    case Pred(t)       ⇒ consts(t)
    case IsZero(t)     ⇒ consts(t)
    case Cond(t, u, v) ⇒ consts(t) ++ consts(u) ++ consts(v)
  }

  def size(t: Term): Int = t match {
    case True | False | Zero ⇒ 1
    case Succ(t)       ⇒ size(t) + 1
    case Pred(t)       ⇒ size(t) + 1
    case IsZero(t)     ⇒ size(t) + 1
    case Cond(t, u, v) ⇒ size(t) + size(u) + size(v) + 1
  }

  def depth(t: Term): Int = t match {
    case True | False | Zero ⇒ 1
    case Succ(t)       ⇒ depth(t) + 1
    case Pred(t)       ⇒ depth(t) + 1
    case IsZero(t)     ⇒ depth(t) + 1
    case Cond(t, u, v) ⇒ List(depth(t), depth(u), depth(v)).max + 1
  }

  def isNumerical(t: Term): Boolean = t match {
    case Zero    ⇒ true
    case Succ(t) ⇒ isNumerical(t)
    case _       ⇒ false
  }

  def isVal(t: Term): Boolean = t match {
    case True | False ⇒ true
    case t            ⇒ isNumerical(t)
  }

  case object NoRuleApplies extends Exception

  def eval1(t: Term): Term = t match {
    case Cond(True,  t2, t3) ⇒ t2
    case Cond(False, t2, t3) ⇒ t3
    case Cond(t1,    t2, t3) ⇒ Cond(eval1(t1), t2, t3)

    case Succ(t1) ⇒ Succ(eval1(t1))

    case Pred(Zero)                          ⇒ Zero
    case Pred(Succ(nv1)) if isNumerical(nv1) ⇒ nv1
    case Pred(t1)                            ⇒ Pred(eval1(t1))

    case IsZero(Zero)                          ⇒ True
    case IsZero(Succ(nv1)) if isNumerical(nv1) ⇒ False
    case IsZero(t1)                            ⇒ IsZero(eval1(t1))

    case _ ⇒ throw NoRuleApplies
  }

  def eval(t: Term): Term = try {
    val result = eval1(t)
    eval(result)
  } catch {
    case NoRuleApplies ⇒ t
  }


}

