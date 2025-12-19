//> using scala 3.7.4

extension (n: Int)
    def mod(d: Int): Int = ((n % d) + d) % d

def parseRotation(line: String): Int =
   line.replaceFirst("^L", "-")
       .replaceFirst("^R", "")
       .toInt

case class Dial(size: Int, pos: Int):
    def turn(rotation: Int) = copy(pos = (pos + rotation).mod(size))

@main
def run =
    val src = io.Source.fromFile("01.txt")
    val rotations =
        try src.getLines.filterNot(_.isEmpty).map(parseRotation).to(Vector)
        finally src.close()

    // zip rotations with their resulting dial states
    val rotationsWithDial: Seq[(rotation: Int, dial: Dial)] =
        val s0 = (rotation = 0, dial = Dial(size = 100, pos = 50))
        rotations.scanLeft(s0): (state, rot) =>
            (rot, state.dial.turn(rot))

    // part 1
    println:
        rotationsWithDial.count(_.dial.pos == 0)

    // part 2
    println:
        rotationsWithDial.map(countPassesThroughZero).sum

def countPassesThroughZero(s: (rotation: Int, dial: Dial)): Int =
    val d =
        if s.rotation >= 0
        then s.dial.pos
        else (s.dial.size - s.dial.pos).mod(s.dial.size)
    z(s.rotation.abs, d, s.dial.size)

def z(r: Int, d: Int, n: Int): Int =
    require(0 <= d && d < n)
    require(0 <= r)
    require(0 < n)
    (r + n - d - 1) / n

