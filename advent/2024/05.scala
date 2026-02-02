//> using scala 3.7.4
//> using dep com.lihaoyi::pprint:0.9.6
//> using dep org.scalameta::munit:1.2.2

// https://adventofcode.com/2024/day/5
// $ scala-cli test 05.scala

case class Problem(
    orderingRules: Seq[(Int, Int)],
    updates: Seq[Seq[Int]],
):
    val sortedUpdates = updates.map: update =>
        val relevantRules = orderingRules.filter: (src, dst) =>
            update.contains(src) && update.contains(dst)
        topoSort(relevantRules)

def readProblem(lines: Seq[String]) =
    Problem(
        orderingRules = lines
            .filter(_.contains("|"))
            .map: line =>
                val ps = line.split("[|]").toList.map(_.toInt)
                (ps(0), ps(1))
    ,   updates = lines
            .filter(_.contains(","))
            .map: line =>
                line.split(",").map(_.toInt)
    )

def topoSort(edges: Seq[(Int, Int)]): Seq[Int] =
    // https://en.wikipedia.org/wiki/Topological_sorting#Kahn's_algorithm
    import collection.mutable.{TreeSet, ArrayBuffer}
    val (allOut, allIn) = edges.unzip
    val nodesWithNoIncomingEdge = allOut.toSet -- allIn.toSet
    val graph = TreeSet(edges*)
    val s = TreeSet(nodesWithNoIncomingEdge.toSeq*)
    val l = ArrayBuffer.empty[Int]
    while s.nonEmpty do
        val n = s.head
        s.remove(n)
        l.append(n)
        for m <- graph.collect { case (`n`, dst) => dst } do
            graph.remove(n -> m)
            if !graph.exists { case (_, dst) => dst == m } then
                s.add(m)
    if graph.nonEmpty then throw Exception("graph has cycles")
    l.toSeq

def part1(problem: Problem) =
    problem.updates
        .zip(problem.sortedUpdates)
        .filter(_ == _)
        .map(_._1)
        .map: update =>
            update(update.size / 2)
        .sum

def part2(problem: Problem) =
    problem.updates
        .zip(problem.sortedUpdates)
        .filter(_ != _)
        .map(_._2)
        .map: update =>
            update(update.size / 2)
        .sum

class Test05 extends munit.FunSuite:

    test("run"):
        val src = io.Source.fromFile("05.txt")
        val problem =
            try readProblem(src.getLines.toSeq)
            finally src.close()
        pprint.log(part1(problem))
        pprint.log(part2(problem))

    val example = readProblem(List(
      "47|53", "97|13", "97|61", "97|47", "75|29", "61|13", "75|53",
      "29|13", "97|29", "53|29", "61|53", "97|53", "61|29", "47|13",
      "75|47", "97|75", "47|61", "75|61", "47|29", "75|13", "53|13",
      "",
      "75,47,61,53,29",
      "97,61,53,29,13",
      "75,29,13",
      "75,97,47,61,53",
      "61,13,29",
      "97,13,75,29,47",
    ))

    test("part1"):
        assertEquals(part1(example), 143)

    test("part2"):
        assertEquals(part2(example), 123)

