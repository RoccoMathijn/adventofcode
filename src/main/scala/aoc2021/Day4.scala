package aoc2021

import util.AocTools
import util.InputGetter.{Live, Mode}

object Day4 extends AocTools(4, 2021) {
//      implicit private val mode: Mode = Example
  implicit private val mode: Mode = Live

  val bingoNumbers: List[Int] = inputLines.head.split(',').map(_.toInt).toList

  case class State(n: Int, called: Boolean)
  type Board = List[List[State]]
  type Boards = List[Board]

  def parseBoard(lines: List[String]): Board =
    lines
      .map(line => List(line.substring(0, 2), line.substring(3, 5), line.substring(6, 8), line.substring(9, 11), line.substring(12, 14)))
      .map(line => line.map(n => State(n.trim.toInt, called = false)))

  val allBoards: Boards = inputLines.drop(2).filter(_.nonEmpty).grouped(5).toList.map(parseBoard)

  def playBingoRound(board: Boards, calling: Int): Boards =
    board.map { board =>
      board.map { row =>
        row.map { number =>
          if (number.n == calling) number.copy(called = true) else number
        }
      }
    }

  def checkForBingo(boards: Boards): Option[Board] = boards.find(isBingo)

  def isBingo(board: Board): Boolean = board.exists(_.forall(_.called)) || board.transpose.exists(_.forall(_.called))

  def playRounds(boards: Boards, bingoNumbers: List[Int]): (Board, Int) = {
    val newBoards = playBingoRound(boards, bingoNumbers.head)
    checkForBingo(newBoards) match {
      case Some(value) => value -> bingoNumbers.head
      case None        => playRounds(newBoards, bingoNumbers.tail)
    }
  }

  def playRounds2(boards: Boards, bingoNumbers: List[Int]): (Board, Int) = {
    val newBoards = playBingoRound(boards, bingoNumbers.head).filterNot(isBingo)
    if (newBoards.size == 1) playRounds(newBoards, bingoNumbers.tail)
    else playRounds2(newBoards, bingoNumbers.tail)
  }

  def solve(board: Board, n: Int): Int = board.map(_.filterNot(_.called).map(_.n).sum).sum * n

  def main(args: Array[String]): Unit = {
    val start = System.currentTimeMillis()
    println(s"AOC 2021 - Day $day")

    val (board, n) = playRounds(allBoards, bingoNumbers)
    val part1 = solve(board, n)
    val mid = System.currentTimeMillis()
    println(s"Answer part 1: $part1 [${mid - start}ms]")
    val (board2, n2) = playRounds2(allBoards, bingoNumbers)
    val part2 = solve(board2, n2)
    val end = System.currentTimeMillis()
    println(s"Answer part 2: $part2 [${end - mid}ms]")
  }
}
