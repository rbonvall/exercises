package tapl.fulluntyped

import scala.language.implicitConversions


/** Ordinary lambda term (with named variables). */
sealed trait Term {
  def $(t: Term) = App(this, t)
}
case class Var(name: Symbol)       extends Term
case class Abs(v: Var, body: Term) extends Term
case class App(f: Term, arg: Term) extends Term

object Term {
  /** Syntactic sugar for lambda abstractions. */
  def λ(x: Symbol, xs: Symbol*)(body: Term) =
    Abs(x, xs.foldRight (body) { (x, acc) ⇒ Abs(x, acc) })
  implicit def symToVar(x: Symbol): Var = Var(x)

  def subst(x: Symbol, s: Term)(t: Term): Term =
    t match {
      case     Var(y) if y == x ⇒ s
      case v @ Var(_)           ⇒ v
      case App(t1, t2) ⇒ subst(x, s)(t1) $ subst(x, s)(t2)
      case Abs(Var(v), t) ⇒ λ(v) { subst(x, s)(t) }
    }
}


/** Nameless lambda term (uses de Bruĳn indices for variables). */
sealed trait NamelessTerm {
  def $(t: NamelessTerm) = NApp(this, t)
}
case class Index(i: Int) extends NamelessTerm
case class NAbs(body: NamelessTerm) extends NamelessTerm
case class NApp(fn: NamelessTerm, arg: NamelessTerm) extends NamelessTerm

object NamelessTerm {
  /** Syntactic sugar for nameless lambda abstractions. */
  def λĳ(body: NamelessTerm) = NAbs(body)
  implicit def intToIndex(i: Int): Index = Index(i)

  /** [Exercise 6.1.5-1] */
  def removeNames(Γ: List[Symbol], t: Term): NamelessTerm =
    t match {
      case Var(x)         ⇒ Index(Γ indexOf x)
      case Abs(Var(x), t) ⇒ NAbs(removeNames(x :: Γ, t))
      case App(f, t)      ⇒ NApp(removeNames(Γ, f), removeNames(Γ, t))
    }

  /** [Exercise 6.1.5-2] */
  def restoreNames(Γ: List[Symbol], nt: NamelessTerm): Term =
    nt match {
      case Index(i)   ⇒ Var(Γ(i))
      case NApp(f, t) ⇒ App(restoreNames(Γ, f), restoreNames(Γ, t))
      case NAbs(t) ⇒
        val x = newVariable(Γ)
        Abs(x, restoreNames(x :: Γ, t))
    }

  /** Returns a variable name that is not in the naming context. */
  def newVariable(Γ: List[Symbol]): Symbol =
    varNames.find { n ⇒ !Γ.contains(n) }.get

  private val allLetters = (('x' to 'z') ++ ('a' to 'w')).toStream
  private def sym(c: Char) = Symbol(c.toString)
  val varNames: Stream[Symbol] = Stream(
    allLetters.map(sym),
    allLetters.map(_.toUpper).map(sym),
    Stream.from(1).map { i ⇒ Symbol(s"x$i") }
  ).flatten

  /** Returns the d-place shift of a nameless term above cutoff c. */
  def shift(d: Int, c: Int = 0)(nt: NamelessTerm): NamelessTerm =
    nt match {
      case Index(k)   ⇒ Index(if (k < c) k else k + d)
      case NAbs(t)    ⇒ NAbs(shift(d, c + 1)(t))
      case NApp(f, t) ⇒ NApp(shift(d, c)(f), shift(d, c)(t))
    }

  /** Substitutes term s for variable with indice j in the term nt. */
  def subst(j: Int, s: NamelessTerm)(nt: NamelessTerm): NamelessTerm =
    nt match {
      case     Index(k) if k == j ⇒ s
      case i @ Index(_)           ⇒ i
      case NAbs(t)    ⇒ NAbs(subst(j + 1, shift(1)(s))(t))
      case NApp(f, t) ⇒ NApp(subst(j, s)(f), subst(j, s)(t))
    }

}
