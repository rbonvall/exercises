// vim: ft=scala

// must be run with ammonite for this import to work
import $file.`10`

val key = "hxtvlmkl"
val inputs = 0 to 127 map { i => s"$key-$i".map(_.toInt).toVector }
val hashes = inputs.map(`10`.hash)

def hexToBin(c: Char): Seq[Int] = {
  val bits = "0123456789abcdef".indexOf(c).toBinaryString
  ("0" * (4 - bits.length) ++ bits).map(_.toString.toInt)
}

def hashToBinary(s: String): Seq[Int] = s.flatMap(hexToBin)

println(hashes.flatMap(hashToBinary).sum)
