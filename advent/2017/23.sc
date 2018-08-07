// vim: ft=scala

trait Ref
case class Literal (value: Int)   extends Ref
case class Register(name: Symbol) extends Ref

import language.implicitConversions
implicit def intToLit(i: Int   ): Ref = Literal(i)
implicit def symToReg(s: Symbol): Ref = Register(s)

trait Instruction
case class Set(reg: Symbol, value: Ref) extends Instruction
case class Sub(reg: Symbol, value: Ref) extends Instruction
case class Mul(reg: Symbol, value: Ref) extends Instruction
case class Jnz(cond: Ref, value: Ref) extends Instruction

val program: Seq[Instruction] = Vector(
  Set('B, 99),
  Set('C, 'B),
  Jnz('A, 2),
  Jnz( 1, 5),
  Mul('B, 100),
  Sub('B, -100000),
  Set('C, 'B),
  Sub('C, -17000),
  Set('F, 1),
  Set('D, 2),
  Set('E, 2),
  Set('G, 'D),
  Mul('G, 'E),
  Sub('G, 'B),
  Jnz('G, 2),
  Set('F, 0),
  Sub('E, -1),
  Set('G, 'E),
  Sub('G, 'B),
  Jnz('G, -8),
  Sub('D, -1),
  Set('G, 'D),
  Sub('G, 'B),
  Jnz('G, -13),
  Jnz('F, 2),
  Sub('H, -1),
  Set('G, 'B),
  Sub('G, 'C),
  Jnz('G, 2),
  Jnz( 1, 3),
  Sub('B, -17),
  Jnz( 1, -23)
)

case class CPU(registers: Map[Symbol, Int], pc: Int, instructions: Seq[Instruction]) {
  def valueOf(ref: Ref) = ref match {
    case Literal (x) ⇒ x
    case Register(r) ⇒ registers(r)
  }
  val isDone = !(0 <= pc && pc < instructions.length)
  def current = instructions(pc)
  def updateRegisterAndJumpToNext(r: Symbol, value: Int) =
    copy(registers = registers.updated(r, value), pc = pc + 1)
  def execute = current match {
    case Set(reg, ref) ⇒ updateRegisterAndJumpToNext(reg, valueOf(ref))
    case Sub(reg, ref) ⇒ updateRegisterAndJumpToNext(reg, valueOf(reg) - valueOf(ref))
    case Mul(reg, ref) ⇒ updateRegisterAndJumpToNext(reg, valueOf(reg) * valueOf(ref))
    case Jnz(cond, ref) if valueOf(cond) == 0 ⇒ copy(pc = pc + 1)
    case Jnz(cond, ref) ⇒
      val offset = if (valueOf(cond) == 0) 1 else valueOf(ref)
      copy(pc = pc + offset)
  }
  def regState =
    "ABCDEFGH".map { c ⇒ registers(Symbol(c.toString)) }
      .mkString("\t")
}
object CPU {
  val initial   = CPU(Map.empty[Symbol, Int] withDefaultValue 0, 0, program)
  val debugMode = CPU(Map('A → 1)            withDefaultValue 0, 0, program)
}

// Part 1
println(
  Iterator.iterate(CPU.initial)(_.execute)
    .takeWhile(!_.isDone)
    .map(_.current)
    .count {
      case Mul(_, _) ⇒ true
      case _         ⇒ false
    }
)

// Part 2
println(
  Iterator.iterate(CPU.debugMode)(_.execute)
    .find(_.isDone)
    .map(_.registers('h))
    .get
)
