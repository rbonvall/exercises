package tapl.fulluntyped

import scala.language.implicitConversions


/** Ordinary lambda term (with named variables). */
enum Term:
  def $(t: Term) = App(this, t)
  case Var(name: String)
  case Abs(v: Var, body: Term)
  case App(f: Term, arg: Term)


object Term:

  given Conversion[String, Var] with
    def apply(s: String): Var = Var(s)

  /** Syntactic sugar for lambda abstractions. */
  def λ(x: String, xs: String*)(body: Term) =
    Abs(x, xs.foldRight (body) { (x, acc) => Abs(x, acc) })

  def subst(x: String, s: Term)(t: Term): Term =
    t match
      case     Var(y) if y == x => s
      case v @ Var(_)           => v
      case     App(t1, t2) => subst(x, s)(t1) $ subst(x, s)(t2)
      case     Abs(Term.Var(v), t) => λ(v) { subst(x, s)(t) }


/** Nameless lambda term (uses de Bruĳn indices for variables). */
enum NamelessTerm :
  def $(t: NamelessTerm) = NApp(this, t)
  case Index(i: Int)
  case NAbs(body: NamelessTerm)
  case NApp(fn: NamelessTerm, arg: NamelessTerm)

object NamelessTerm:

  given Conversion[Int, NamelessTerm] with
    def apply(i: Int): NamelessTerm = Index(i)

  /** Syntactic sugar for nameless lambda abstractions. */
  def λĳ(body: NamelessTerm) = NAbs(body)

  /** [Exercise 6.1.5-1] */
  def removeNames(Γ: List[String], t: Term): NamelessTerm =
    t match
      case Term.Var(x)              => Index(Γ indexOf x)
      case Term.Abs(Term.Var(x), t) => NAbs(removeNames(x :: Γ, t))
      case Term.App(f, t)           => NApp(removeNames(Γ, f), removeNames(Γ, t))

  /** [Exercise 6.1.5-2] */
  def restoreNames(Γ: List[String], nt: NamelessTerm): Term =
    nt match
      case Index(i)   => Term.Var(Γ(i))
      case NApp(f, t) => Term.App(restoreNames(Γ, f), restoreNames(Γ, t))
      case NAbs(t) =>
        val x = newVariable(Γ)
        Term.Abs(x, restoreNames(x :: Γ, t))

  /** Returns a variable name that is not in the naming context. */
  def newVariable(Γ: List[String]): String =
    varNames.find { n => !Γ.contains(n) }.get

  private val allLetters = (('x' to 'z') ++ ('a' to 'w')).to(LazyList)
  val varNames: LazyList[String] = LazyList(
    allLetters.map(_.toString),
    allLetters.map(_.toUpper).map(_.toString),
    LazyList.from(1).map { i => s"x$i" }
  ).flatten

  /** Returns the d-place shift of a nameless term above cutoff c. */
  def shift(d: Int, c: Int = 0)(nt: NamelessTerm): NamelessTerm =
    nt match
      case Index(k)   => Index(if (k < c) k else k + d)
      case NAbs(t)    => NAbs(shift(d, c + 1)(t))
      case NApp(f, t) => NApp(shift(d, c)(f), shift(d, c)(t))

  /** Substitutes term s for variable with indice j in the term nt. */
  def subst(j: Int, s: NamelessTerm)(nt: NamelessTerm): NamelessTerm =
    nt match
      case     Index(k) if k == j => s
      case i @ Index(_)           => i
      case NAbs(t)    => NAbs(subst(j + 1, shift(1)(s))(t))
      case NApp(f, t) => NApp(subst(j, s)(f), subst(j, s)(t))

