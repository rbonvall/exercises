// vim: ft=scala

import math.abs

sealed trait Dir
case object N  extends Dir
case object S  extends Dir
case object NW extends Dir
case object SW extends Dir
case object NE extends Dir
case object SE extends Dir
object Dir {
  val fromString: PartialFunction[String, Dir] = {
    case "n"  ⇒ N
    case "s"  ⇒ S
    case "nw" ⇒ NW
    case "sw" ⇒ SW
    case "ne" ⇒ NE
    case "se" ⇒ SE
  }
}

case class HexPos(i: Int, j: Int) {
  def to(dir: Dir): HexPos = dir match {
    case N  ⇒ copy(i = i + 1, j = j + 1)
    case S  ⇒ copy(i = i - 1, j = j - 1)
    case NW ⇒ copy(           j = j + 1)
    case SW ⇒ copy(i = i - 1           )
    case NE ⇒ copy(i = i + 1           )
    case SE ⇒ copy(           j = j - 1)
  }

  def distToOrigin =
    if (i * j > 0) abs(i) max abs(j)
    else           abs(i)  +  abs(j)
}
object HexPos {
  val origin = HexPos(0, 0)
}

val path: Seq[Dir] =
  scala.io.Source.fromFile("11.txt")
    .mkString
    .split(",")
    .toVector
    .map(_.trim.toLowerCase)
    .map(Dir.fromString)

val moves = path.scanLeft (HexPos.origin) { (pos, step) ⇒ pos to step }

val dest    = moves.last
val maxDest = moves.maxBy(_.distToOrigin)

println(dest   .distToOrigin)
println(maxDest.distToOrigin)



