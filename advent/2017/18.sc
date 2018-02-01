// vim: ft=scala

trait Ref { def repr: String }
case class Literal (value: Long)  extends Ref { val repr = value.toString }
case class Register(name: Symbol) extends Ref { val repr = name.name }

import language.implicitConversions
implicit def intToLit(i: Long  ): Ref = Literal(i)
implicit def symToReg(s: Symbol): Ref = Register(s)

trait Instruction {
  override def toString = this match {
    case Set(reg, value)  ⇒ s"set ${reg.name} ${value.repr}"
    case Add(reg, value)  ⇒ s"add ${reg.name} ${value.repr}"
    case Mod(reg, value)  ⇒ s"mod ${reg.name} ${value.repr}"
    case Mul(reg, value)  ⇒ s"mul ${reg.name} ${value.repr}"
    case Jgz(cond, value) ⇒ s"jgz ${cond.repr} ${value.repr}"
    case Snd(value)       ⇒ s"snd ${value.repr}"
    case Rcv(value)       ⇒ s"rcv ${value.repr}"
  }
}
case class Set(reg: Symbol, value: Ref) extends Instruction
case class Add(reg: Symbol, value: Ref) extends Instruction
case class Mod(reg: Symbol, value: Ref) extends Instruction
case class Mul(reg: Symbol, value: Ref) extends Instruction
case class Jgz(cond: Ref, value: Ref) extends Instruction
case class Snd(value: Ref) extends Instruction
case class Rcv(value: Ref) extends Instruction

val input: IndexedSeq[Instruction] = Vector(
  Set('I,  31),
  Set('A,   1),
  Mul('P,  17),
  Jgz('P,  'P),
  Mul('A,   2),
  Add('I,  -1),
  Jgz('I,  -2),
  Add('A,  -1),
  Set('I, 127),
  Set('P, 680),
  Mul('P,   8505),
  Mod('P,  'A),
  Mul('P, 129749),
  Add('P,  12345),
  Mod('P,  'A),
  Set('B,  'P),
  Mod('B,  10000),
  Snd('B),
  Add('I,  -1),
  Jgz('I,  -9),
  Jgz('A,   3),
  Rcv('B),
  Jgz('B,  -1),
  Set('F,   0),
  Set('I, 126),
  Rcv('A),
  Rcv('B),
  Set('P,  'A),
  Mul('P,  -1),
  Add('P,  'B),
  Jgz('P,   4),
  Snd('A),
  Set('A,  'B),
  Jgz( 1,   3),
  Snd('B),
  Set('F,   1),
  Add('I,  -1),
  Jgz('I, -11),
  Snd('A),
  Jgz('F, -16),
  Jgz('A, -19)
)

case class State(registers: Map[Symbol, Long],
                 instructions: IndexedSeq[Instruction],
                 pc: Int,
                 lastFreq: Long) {

  def valueOf(ref: Ref) = ref match {
    case Literal (x) ⇒ x
    case Register(r) ⇒ registers(r)
  }

  def play(freq: Long) = copy(lastFreq = freq, pc = pc + 1)
  def reg(c: Symbol)(f: Long ⇒ Long) = copy(registers = registers.updated(c, f(registers(c))), pc = pc + 1)

  val isDone = !(0 <= pc && pc < instructions.length)
  def current = instructions(pc)
  def next = current match {
    case Snd(x)     ⇒ play(valueOf(x))
    case Set(r, y)  ⇒ reg(r) { _ ⇒ valueOf(y) }
    case Add(r, y)  ⇒ reg(r) { _ + valueOf(y) }
    case Mul(r, y)  ⇒ reg(r) { _ * valueOf(y) }
    case Mod(r, y)  ⇒ reg(r) { n ⇒ ((n % valueOf(y)) + valueOf(y)) % valueOf(y) }
    case Rcv(x)     ⇒ copy(pc = pc + 1)
    case Jgz(x, offset)  ⇒
      val d = if (valueOf(x) > 0) valueOf(offset) else 1
      copy(pc = pc + d.toInt)
  }

  override def toString = {
    val rs = registers.toSeq
      .filter { case (k, v) ⇒ v != 0 }
      .sortBy { case (k, v) ⇒ k.name }
      .map    { case (k, v) ⇒ s"${k.name}:$v" }
      .mkString("\t")
    s"State(pc=$pc [$current], last=$lastFreq,\t$rs)"
  }
}
object State {
  val initial = State(Map.empty withDefaultValue 0L, input, 0, 0L)
}

Stream.iterate(State.initial)(_.next)
  .takeWhile(!_.isDone)
  .filter(s => s.current.isInstanceOf[Rcv] || s.current.isInstanceOf[Snd])
  .take(1000)
  .toList foreach println

