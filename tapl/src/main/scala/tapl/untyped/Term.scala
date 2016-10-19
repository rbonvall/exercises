package tapl.untyped

sealed trait Info
case object DummyInfo extends Info

sealed trait Binding
case object NameBind extends Binding

sealed trait Term {
  def repr(ctx: Term.Context): String = this match {
    case Term.Var(n)          ⇒ indexToName(ctx, n)
    case Term.App(fn, arg)    ⇒ s"(${fn.repr(ctx)} ${arg.repr(ctx)})"
    case Term.Abs(body, name) ⇒
      val (ctx2, x) = pickFreshName(ctx, name)
      s"(λ$x . ${body.repr(ctx2)})"
  }

  def pickFreshName(ctx: Term.Context, name: String): (Term.Context, String) = ???
  def indexToName(ctx: Term.Context, n: Int): String = ???
}
object Term {
  // n is the de Bruijn index
  case class Var(n: Int)                   extends Term
  case class Abs(body: Term, name: String) extends Term
  case class App(fn: Term, arg: Term)      extends Term

  type Context = List[(String, Binding)]
}


