// vim: ft=scala

val input: IndexedSeq[Seq[String]] =
  """set i 31
    |set a 1
    |mul p 17
    |jgz p p
    |mul a 2
    |add i -1
    |jgz i -2
    |add a -1
    |set i 127
    |set p 680
    |mul p 8505
    |mod p a
    |mul p 129749
    |add p 12345
    |mod p a
    |set b p
    |mod b 10000
    |snd b
    |add i -1
    |jgz i -9
    |jgz a 3
    |rcv b
    |jgz b -1
    |set f 0
    |set i 126
    |rcv a
    |rcv b
    |set p a
    |mul p -1
    |add p b
    |jgz p 4
    |snd a
    |set a b
    |jgz 1 3
    |snd b
    |set f 1
    |add i -1
    |jgz i -11
    |snd a
    |jgz f -16
    |jgz a -19""".stripMargin.trim.lines.toIndexedSeq.map(_.trim.split("\\s+").toVector)

val registers = Map.empty[Char, Int] withDefaultValue 0

object Int {
  def unapply(s: String) = scala.util.Try(s.toInt).toOption
}
object Reg {
  val regs = 'a' to 'z'
  def unapply(s: String) = if (s.length == 1 && regs.contains(s.head)) Some(s.head) else None
}

case class State(registers: Map[Char, Int],
                 instructions: IndexedSeq[Seq[String]],
                 pc: Int = 0,
                 lastFreq: Option[Int] = None) {

  def valOf(s: String): Int = Int.unapply(s) getOrElse registers(s.head)

  def reg(c: Char)(f: Int ⇒ Int) = copy(registers = registers.updated(c, f(registers(c))), pc = pc + 1)
  def play(freq: Int) = copy(lastFreq = Some(freq), pc = pc + 1)

  val currentInstruction = instructions.lift(pc)
  def next: Option[State] = currentInstruction map {
    case Seq("snd", x)          ⇒ play(valOf(x))
    case Seq("set", Reg(r), y)  ⇒ reg(r) { _ ⇒ valOf(y) }
    case Seq("add", Reg(r), y)  ⇒ reg(r) { _ + valOf(y) }
    case Seq("mul", Reg(r), y)  ⇒ reg(r) { _ * valOf(y) }
    case Seq("mod", Reg(r), y)  ⇒ reg(r) { n ⇒ ((n % valOf(y)) + valOf(y)) % valOf(y) }
    case Seq("rcv", x)          ⇒ copy(pc = pc + 1)
    case Seq("jgz", x, offset)  ⇒ if (valOf(x) > 0) copy(pc = pc + valOf(offset))
                                  else copy(pc = pc + 1)
    case _ ⇒ ???
  }
}
val initialState = State(Map.empty withDefaultValue 0, input, 0, None)

def showState(s: State): String = {
  s.registers
    .map { case (k, v) ⇒ s"$k: $v" }
    .mkString(",")
    .++(s" ${s.currentInstruction.map(_.mkString(" "))} ${s.lastFreq}")
}

//var s = initialState
//for (i ← Stream from 0 take 1000) {
//  if (i > 95) println(s"[$i] ${showState(s)}")
//  s = s.next.get
//}

Stream.iterate(Option(initialState)) { _.flatMap(_.next) }
  .takeWhile(_.isDefined)
  .filter { s ⇒ s.exists(_.currentInstruction.exists(_.startsWith("rcv"))) }
  .take(10)
  .toList foreach println

