package tapl.untyped

enum Info:
  case DummyInfo

enum Binding:
  case NameBind

enum Term:
  // n is the de Bruijn index
  case Var(n: Int)
  case Abs(body: Term, name: String)
  case App(fn: Term, arg: Term)

  def repr(ctx: Term.Context): String = this match
    case Term.Var(n)          => Term.indexToName(ctx, n)
    case Term.App(fn, arg)    => s"(${fn.repr(ctx)} ${arg.repr(ctx)})"
    case Term.Abs(body, name) =>
      val (ctx2, x) = Term.pickFreshName(ctx, name)
      s"(λ$x . ${body.repr(ctx2)})"

object Term:
  def pickFreshName(ctx: Term.Context, name: String): (Term.Context, String) = ???
  def indexToName(ctx: Term.Context, n: Int): String = ???

  type Context = List[(String, Binding)]


