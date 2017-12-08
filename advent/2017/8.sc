// vim: ft=scala

case class Instruction(reg: String, op: String, delta: Int, cmpReg: String, cmp: String, refVal: Int)

def lineToInstruction(line: String) = {
  val parts = line.split(" ").toSeq
  Instruction(parts(0), parts(1), parts(2).toInt, parts(4), parts(5), parts(6).toInt)
}

val input = scala.io.Source.fromFile("8.txt").getLines.map(lineToInstruction).toStream

val initialState = Map.empty[String, Int] withDefaultValue 0

val allStates =
  input.scanLeft (initialState) { (state, instruction) ⇒
    val cmpFunc: (Int, Int) ⇒ Boolean = instruction.cmp match {
      case "<"  ⇒ _ <  _
      case ">"  ⇒ _ >  _
      case "<=" ⇒ _ <= _
      case ">=" ⇒ _ >= _
      case "==" ⇒ _ == _
      case "!=" ⇒ _ != _
    }
    val op: (Int, Int) ⇒ Int = instruction.op match {
      case "inc" ⇒ _ + _
      case "dec" ⇒ _ - _
    }
    val cond = cmpFunc(state(instruction.cmpReg), instruction.refVal)
    val reg = instruction.reg
    if (cond) state.updated(reg, op(state(reg), instruction.delta))
    else      state
  }

println(allStates.last.values.max)
println(allStates.flatMap(_.values).max)
