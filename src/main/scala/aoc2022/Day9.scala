package aoc2022

import util.AocTools
import util.InputGetter.{Example, Live, Mode}

object Day9 extends AocTools(9, 2022) {
//  implicit private val mode: Mode = Example
  implicit private val mode: Mode = Live

  case class Pos(x: Int, y: Int)
  case class State(rope: List[Pos], tailHistory: Set[Pos])

  def buildHistory(ropeSize: Int): State =
    inputLines.foldLeft(State(List.fill(ropeSize)(Pos(0, 0)), Set.empty)) {
      case (State(h :: body, history), s"R $x") =>
        val hPath = (1 to x.toInt).map(x => h.copy(x = h.x + x)).toList
        val bodyPaths = motion(hPath, body)
        State(hPath.last :: bodyPaths.head, history ++ bodyPaths.map(_.last))
      case (State(h :: body, history), s"L $x") =>
        val hPath = (1 to x.toInt).map(x => h.copy(x = h.x - x)).toList
        val bodyPaths = motion(hPath, body)
        State(hPath.last :: bodyPaths.head, history ++ bodyPaths.map(_.last))
      case (State(h :: body, history), s"U $y") =>
        val hPath = (1 to y.toInt).map(y => h.copy(y = h.y + y)).toList
        val bodyPaths = motion(hPath, body)
        State(hPath.last :: bodyPaths.head, history ++ bodyPaths.map(_.last))
      case (State(h :: body, history), s"D $y") =>
        val hPath = (1 to y.toInt).map(y => h.copy(y = h.y - y)).toList
        val bodyPaths = motion(hPath, body)
        State(hPath.last :: bodyPaths.head, history ++ bodyPaths.map(_.last))
    }

  def motion(headPath: List[Pos], body: List[Pos]): List[List[Pos]] =
    headPath.foldLeft(List(body))((bodyHistory, head) => step(head, bodyHistory.head) :: bodyHistory)

  def step(headPosition: Pos, body: List[Pos]): List[Pos] =
    body
      .foldLeft(List(headPosition)) { (newBody, follower) =>
        follow(newBody.head, follower) :: newBody
      }
      .reverse
      .drop(1)

  def follow(hPos: Pos, tPos: Pos): Pos = {
    val xDistance = math.abs(hPos.x - tPos.x)
    val yDistance = math.abs(hPos.y - tPos.y)
    (xDistance, yDistance) match {
      case (2, 0) => Pos((hPos.x + tPos.x) / 2, tPos.y) // move horizontal
      case (0, 2) => Pos(tPos.x, (hPos.y + tPos.y) / 2) // move vertically
      case (2, 1) => Pos((hPos.x + tPos.x) / 2, hPos.y) // move diagonally on x axis
      case (1, 2) => Pos(hPos.x, (hPos.y + tPos.y) / 2) // move diagonally on y axis
      case (2, 2) => Pos((hPos.x + tPos.x) / 2, (hPos.y + tPos.y) / 2) // move diagonally on both axes
      case _      => tPos
    }
  }

  def solve1: Int = buildHistory(2).tailHistory.size
  def solve2: Int = buildHistory(10).tailHistory.size

  def main(args: Array[String]): Unit = {
    val start = System.currentTimeMillis()
    println(s"AOC 2022 - Day $day")

    val part1 = solve1
    val mid = System.currentTimeMillis()
    println(s"Answer part 1: $part1 [${mid - start}ms]")
    val part2 = solve2
    val end = System.currentTimeMillis()
    println(s"Answer part 2: $part2 [${end - mid}ms]")
  }
}
