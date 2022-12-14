package aoc2022

import aoc2022.Prelude.Point
import util.AocTools
import util.InputGetter.{Live, Mode}

object Day14 extends AocTools(14, 2022) {
//      implicit private val mode: Mode = Example
  implicit private val mode: Mode = Live

  val cave: Set[Point] = inputLines.flatMap { line =>
    line.split(" -> ").sliding(2).map(_.map(_.split(",").map(_.toInt))).flatMap {
      case Array(Array(startX, startY), Array(endX, endY)) =>
        for {
          x <- math.min(startX, endX) to math.max(startX, endX)
          y <- math.min(startY, endY) to math.max(startY, endY)
        } yield Point(x, y)
    }
  }.toSet

  val maxY: Int = cave.map(_.y).max

  def dropSand(fallingSand: Point, sandSpots: Set[Point], withBottom: Boolean): Set[Point] = {
    val List(leftD, down, rightD) = (-1 to 1).map(x => Point(fallingSand.x + x, fallingSand.y + 1)).toList
    if (sandSpots.contains(Point(500, 0))) sandSpots
    else if (bottom(fallingSand)) if (withBottom) sandSpots + fallingSand else sandSpots
    else if (unoccupied(down, sandSpots)) dropSand(down, sandSpots, withBottom)
    else if (unoccupied(leftD, sandSpots)) dropSand(leftD, sandSpots, withBottom)
    else if (unoccupied(rightD, sandSpots)) dropSand(rightD, sandSpots, withBottom)
    else sandSpots + fallingSand
  }

  private def unoccupied(point: Point, sandSpots: Set[Point]): Boolean = !cave.contains(point) && !sandSpots.contains(point)

  def bottom(point: Point): Boolean = point.y == maxY + 1

  def dropUntilFull(sandSpots: Set[Point], withBottom: Boolean): Set[Point] = {
    val newSandSpots = dropSand(Point(500, 0), sandSpots, withBottom)
    if (newSandSpots == sandSpots) sandSpots
    else dropUntilFull(newSandSpots, withBottom)
  }

  def printCave(sandSpots: Set[Point]): Unit = {
    (math.min(sandSpots.map(_.y).min, cave.map(_.y).min) to math.max(sandSpots.map(_.y).max, cave.map(_.y).max)).foreach { y =>
      (math.min(sandSpots.map(_.x).min, cave.map(_.x).min) to math.max(sandSpots.map(_.x).max, cave.map(_.x).max)).foreach { x =>
        if (cave.contains(Point(x, y))) print('#')
        else if (sandSpots.contains(Point(x, y))) print('O')
        else print('.')
      }
      println("")
    }
  }

  def solve1: Int = dropUntilFull(Set.empty, withBottom = false).size
  def solve2: Int = dropUntilFull(Set.empty, withBottom = true).size

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
