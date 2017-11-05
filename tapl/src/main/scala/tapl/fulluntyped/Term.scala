package tapl.fulluntyped

sealed trait Term {
  def $(t: Term) = App(this, t)
}
object Term {
  implicit def symToVar(x: Symbol): Var = Var(x)
  def λ(x: Symbol, xs: Symbol*)(body: Term) =
    Abs(x, xs.foldRight (body) { (x, acc) ⇒ Abs(x, acc) })
}
case class Var(name: Symbol)       extends Term
case class Abs(v: Var, body: Term) extends Term
case class App(f: Term, arg: Term) extends Term


sealed trait NamelessTerm {
  def $(t: NamelessTerm) = NApp(this, t)
}
object NamelessTerm {
  def λĳ(body: NamelessTerm) = NAbs(body)
  implicit def intToIndex(i: Int): Index = Index(i)
}
case class Index(i: Int) extends NamelessTerm
case class NAbs(body: NamelessTerm) extends NamelessTerm
case class NApp(fn: NamelessTerm, arg: NamelessTerm) extends NamelessTerm


object DeBruĳn {

  def removeNames(Γ: List[Symbol], t: Term): NamelessTerm =
    t match {
      case Var(x)         ⇒ Index(Γ indexOf x)
      case Abs(Var(x), t) ⇒ NAbs(removeNames(x :: Γ, t))
      case App(f, t)      ⇒ NApp(removeNames(Γ, f), removeNames(Γ, t))
    }

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
  private val varNames: Stream[Symbol] = Stream(
    allLetters.map(sym),
    allLetters.map(_.toUpper).map(sym),
    Stream.from(1).map { i ⇒ Symbol(s"x$i") }
  ).flatten

  /** Returns the d-place shift of a nameless term above cutoff c. */
  def shift(d: Int, c: Int = 0)(nt: NamelessTerm): NamelessTerm =
    nt match {
      case Index(k)   ⇒ Index(if (k < c) k else k + d)
      case NAbs(t)    ⇒ NAbs(shift(d, c + 1)(nt))
      case NApp(f, t) ⇒ NApp(shift(d, c)(f), shift(d, c)(t))
    }

}
