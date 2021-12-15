import scala.io.Source
import java.io.File

case class BoardState(unmarkedRowsAndColumns: List[Set[Int]], lastDrawn: Option[Int]):
  def draw(n: Int) =
    BoardState(unmarkedRowsAndColumns.map(_ - n), Some(n))
  def hasWon =
    unmarkedRowsAndColumns.exists(_.isEmpty)
  def allUnmarked =
    unmarkedRowsAndColumns.reduce(_ union _)

object BoardState:
  def fromRows(rows: List[List[Int]]) =
    BoardState(rows.map(_.toSet) ++ rows.transpose.map(_.toSet), None)

def computeAllGameStates(drawn: List[Int], initialBoardStates: List[BoardState]): LazyList[List[BoardState]] =
  drawn.to(LazyList).scanLeft(initialBoardStates) { (previousState, n) =>
    previousState.map(_.draw(n))
  }

def part1(drawn: List[Int], boards: List[BoardState]) =
  val allBoardStates: LazyList[BoardState] = computeAllGameStates(drawn, boards).flatten
  val scoreOpt =
    for
      winningBoard  <- allBoardStates.find(_.hasWon)
      lastDrawnNum  <- winningBoard.lastDrawn
    yield winningBoard.allUnmarked.sum * lastDrawnNum
  scoreOpt.getOrElse(-1)

def part2(drawn: List[Int], boards: List[BoardState]) =
  val allGameStates: LazyList[List[BoardState]] = computeAllGameStates(drawn, boards)
  val scoreOpt =
    for
      gameStateWithLoser <- allGameStates.find(_.count(!_.hasWon) == 1)
      losingBoard        <- gameStateWithLoser.find(!_.hasWon)

      // Replay the losingBoard to find its own winning state
      losingBoardStates       =  computeAllGameStates(drawn, List(losingBoard)).flatten
      losingBoardWinningState <- losingBoardStates.find(_.hasWon)
      lastDrawnNum            <- losingBoardWinningState.lastDrawn
    yield losingBoardWinningState.allUnmarked.sum * lastDrawnNum
  scoreOpt.getOrElse(-1)

@main
def run =
  val src = Source.fromFile(File("04.txt"))
  val inputBlocks: List[String] =
    try src.mkString.split("\n\n").toList
    finally src.close()

  val drawn: List[Int] = inputBlocks.head.split(",").map(_.toInt).toList
  val boards: List[List[List[Int]]] = inputBlocks.tail
    .map(block =>
        block.split("\n").toList.map(line =>
            line.split(" ").toList.filter(_.nonEmpty).map(_.toInt)
        )
    )

  val initalBoardStates = boards.map(BoardState.fromRows)

  println(part1(drawn, initalBoardStates))
  println(part2(drawn, initalBoardStates))
