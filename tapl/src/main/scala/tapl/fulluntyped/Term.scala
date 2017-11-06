package tapl.fulluntyped

import scala.language.implicitConversions

/** Ordinary lambda term (with named variables). */
sealed trait Term {
  def $(t: Term) = App(this, t)
}
object Term {
  /** Syntactic sugar for lambda abstractions. */
  def λ(x: Symbol, xs: Symbol*)(body: Term) =
    Abs(x, xs.foldRight (body) { (x, acc) ⇒ Abs(x, acc) })
  implicit def symToVar(x: Symbol): Var = Var(x)
}
case class Var(name: Symbol)       extends Term
case class Abs(v: Var, body: Term) extends Term
case class App(f: Term, arg: Term) extends Term

/** Nameless lambda term (uses de Bruĳn indices for variables). */
sealed trait NamelessTerm {
  def $(t: NamelessTerm) = NApp(this, t)
}
object NamelessTerm {
  /** Syntactic sugar for nameless lambda abstractions. */
  def λĳ(body: NamelessTerm) = NAbs(body)
  implicit def intToIndex(i: Int): Index = Index(i)
}
case class Index(i: Int) extends NamelessTerm
case class NAbs(body: NamelessTerm) extends NamelessTerm
case class NApp(fn: NamelessTerm, arg: NamelessTerm) extends NamelessTerm


object DeBruĳn {

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

}
