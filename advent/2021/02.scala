import scala.io.Source
import java.io.File

enum Direction:
  case Forward, Down, Up

import Direction.*

case class Command(direction: Direction, units: Int)

object Command:
  val forwardRegex = """forward (\d+)""".r
  val downRegex    = """down (\d+)""".r
  val upRegex      = """up (\d+)""".r
  def parse(line: String): Command =
    line match
      case forwardRegex(u) => Command(Forward, u.toInt)
      case downRegex(u)    => Command(Down,    u.toInt)
      case upRegex(u)      => Command(Up,      u.toInt)

case class State(hPos: Int, depth: Int):
  def update(cmd: Command): State =
    cmd match
      case Command(Forward, u) => State(hPos + u, depth)
      case Command(Down,    u) => State(hPos, depth + u)
      case Command(Up,      u) => State(hPos, depth - u)
  def product = hPos * depth

object State:
  val initial = State(0, 0)

def part1(cmds: List[Command]) =
  val finalState = cmds.foldLeft(State.initial)(_ update _)
  finalState.product

case class StateWithAim(hPos: Int, depth: Int, aim: Int):
  def update(cmd: Command): StateWithAim =
    cmd match
      case Command(Forward, x) => StateWithAim(hPos + x, depth + aim * x, aim)
      case Command(Down,    x) => StateWithAim(hPos, depth, aim + x)
      case Command(Up,      x) => StateWithAim(hPos, depth, aim - x)
  def product = hPos * depth

object StateWithAim:
  val initial = StateWithAim(0, 0, 0)

def part2(cmds: List[Command]) =
  val finalState = cmds.foldLeft(StateWithAim.initial)(_ update _)
  finalState.product

@main
def run =
  val src = Source.fromFile(File("02.txt"))
  val input: List[Command] =
    try src.getLines.map(Command.parse).toList
    finally src.close()
  assert(input.length == 1000)

  println(part1(input))
  println(part2(input))
