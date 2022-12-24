package aoc2022

import aoc2022.Prelude.Point
import util.AocTools
import util.InputGetter._

object Day24 extends AocTools(24, 2022) {
//  implicit private val mode: Mode = Example
  implicit private val mode: Mode = Live

  val input = inputLines.map(_.toList)
  type Blizzard = (Char, Point)
  val blizzards = input.zipWithIndex
    .flatMap {
      case (row, y) =>
        row.zipWithIndex
          .collect { case (c, x) if c != '#' && c != '.' => c -> Point(x, y) }
    }

  val maxX = input.head.size - 1
  val maxY = input.size - 1
  val startPos = Point(1, 0)
  val endPos = Point(maxX - 1, maxY)

  def traverse(currPositions: Set[Point], blizzards: List[Blizzard], steps: Int, to: Point): (Int, List[Blizzard]) = {
    if (currPositions.contains(to) || steps > 500) steps -> blizzards
    else {
      val nextSteps = (currPositions.flatMap(Prelude.fourAdjacencies) ++ currPositions)
        .filterNot(p => p != startPos && p.y <= 0)
        .filterNot(p => p != endPos && p.y >= maxY)
        .filterNot(p => p.x <= 0 || p.x >= maxX)
      val nextB = nextBlizzards(blizzards)
      val filteredSteps = nextSteps.diff(nextB.map(_._2).toSet)
      traverse(filteredSteps, nextB, steps + 1, to)
    }
  }

  def nextBlizzards(blizzards: List[Blizzard]): List[Blizzard] = {
    blizzards.map {
      case ('>', point) => '>' -> wrapAround(point.copy(x = point.x + 1))
      case ('<', point) => '<' -> wrapAround(point.copy(x = point.x - 1))
      case ('^', point) => '^' -> wrapAround(point.copy(y = point.y - 1))
      case ('v', point) => 'v' -> wrapAround(point.copy(y = point.y + 1))
    }
  }

  def wrapAround(point: Point): Point = {
    if (point.x == 0) point.copy(x = maxX - 1)
    else if (point.y == 0) point.copy(y = maxY - 1)
    else if (point.y == maxY) point.copy(y = 1)
    else if (point.x == maxX) point.copy(x = 1)
    else point
  }

  def solve1: Int = traverse(Set(startPos), blizzards, 0, endPos)._1

  def solve2: Int = {
    val (one, blizzards1) = traverse(Set(startPos), blizzards, 0, endPos)
    val (two, blizzards2) = traverse(Set(endPos), blizzards1, 0, startPos)
    val (three, _) = traverse(Set(startPos), blizzards2, 0, endPos)
    one + two + three
  }

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
