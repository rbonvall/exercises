//> using scala 3.5
//> using dep org.scalameta::munit:1.0.2
//> using option -deprecation

class MapRange(destStart: Long, srcStart: Long, length: Long):
    val srcRange = srcStart until (srcStart + length)
    val dx = destStart - srcStart
    val asPf: PartialFunction[Long, Long] = { case x if srcRange contains(x) => x + dx }

class ConversionMap(val ranges: List[MapRange]):
    val asFunc: (Long => Long) = ranges
        .map(_.asPf)
        .reduce(_ orElse _)
        .orElse(x => x)
    def apply(x: Long) = asFunc(x)

case class Almanac(seeds: List[Long], maps: List[ConversionMap]):
    val asFunc = maps.map(_.asFunc).reduce(_ andThen _)
    def processSeed(seed: Long) = asFunc(seed)

def lineBlocks(s: String) =
    s.split("\n\n")
        .to(List)
        .map: block =>
            block.split("\n").to(List)

def parseAlmanac(blocks: List[List[String]]) =
    val seeds = blocks(0)(0).split(" ").drop(1).map(_.toLong).to(List)
    val maps = blocks.drop(1).map: blockLines =>
        val ranges = blockLines.drop(1).map: line =>
            val rangeValue = line.split(" ").map(_.toLong)
            MapRange(rangeValue(0), rangeValue(1), rangeValue(2))
        ConversionMap(ranges)
    Almanac(seeds, maps)

def composeMaps(m1: ConversionMap, m2: ConversionMap): ConversionMap =
    val rs = Nil
    ConversionMap(rs)
    // TODO: implement
    m1


@main
def run =
    val src = io.Source.fromFile("05.txt")
    val blocks = 
        try lineBlocks(src.mkString)
        finally src.close()

    val almanac = parseAlmanac(blocks)
    println(part1(almanac))
    println(part2(almanac))

def part1(almanac: Almanac) =
    almanac.seeds.map(almanac.processSeed).min

def part2(almanac: Almanac) =
    almanac.seeds.grouped(2)
        .collect:
            case s0 :: n :: Nil => s0 until (s0 + n)
        .map: seedRange =>
            seedRange.map(almanac.processSeed).min
        .min


class Tests05 extends munit.FunSuite:

    val example =
      """seeds: 79 14 55 13
        |
        |seed-to-soil map:
        |50 98 2
        |52 50 48
        |
        |soil-to-fertilizer map:
        |0 15 37
        |37 52 2
        |39 0 15
        |
        |fertilizer-to-water map:
        |49 53 8
        |0 11 42
        |42 0 7
        |57 7 4
        |
        |water-to-light map:
        |88 18 7
        |18 25 70
        |
        |light-to-temperature map:
        |45 77 23
        |81 45 19
        |68 64 13
        |
        |temperature-to-humidity map:
        |0 69 1
        |1 0 69
        |
        |humidity-to-location map:
        |60 56 37
        |56 93 4""".stripMargin


    lazy val exampleAlmanac = parseAlmanac(lineBlocks(example))

    test("parseAlmanac"):
        assertEquals(exampleAlmanac.seeds, List(79L, 14L, 55L, 13L))
        assertEquals(exampleAlmanac.maps.length, 7)

    test("Almanac#process"):
        assertEquals(
            exampleAlmanac.seeds.map(exampleAlmanac.processSeed),
            List(82L, 43L, 86L, 35L)
        )

    test("composeMaps"):
        val exampleMapPairs = List(
            (exampleAlmanac.maps(0), exampleAlmanac.maps(1)),
            (exampleAlmanac.maps(2), exampleAlmanac.maps(4)),
            (exampleAlmanac.maps(4), exampleAlmanac.maps(2)),
        )
        for (m1, m2) <- exampleMapPairs do
            val mc = composeMaps(m1, m2)
            for i <- 0 to 100 do
                assertEquals(mc(i), m1(m2(i)))

    test("part1"):
        assertEquals(part1(exampleAlmanac), 35L)

    test("part2"):
        assertEquals(part2(exampleAlmanac), 46L)

