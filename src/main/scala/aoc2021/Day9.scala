package aoc2021

import util.AocTools
import util.InputGetter.{Live, Mode}

object Day9 extends AocTools(9, 2021) {
//  implicit private val mode: Mode = Example
  implicit private val mode: Mode = Live

  lazy val input: List[List[Int]] = inputLines.map(_.toList.map(_.toString.toInt))

  case class Point(x: Int, y: Int)

  lazy val allPoints: Set[Point] = input.head.indices.flatMap(x => input.indices.map(y => Point(x, y))).toSet

  lazy val lowPoints: List[Int] = allPoints.toList.collect { case point if isLowPoint(point) => valueOf(point) }

  def valueOf(point: Point): Int = input(point.y)(point.x)

  def neighbouringPoints(point: Point): Set[Point] =
    (point match {
      case Point(x, y) => Set(Point(x, y - 1), Point(x - 1, y), Point(x + 1, y), Point(x, y + 1))
    }).intersect(allPoints)

  def isLowPoint(point: Point): Boolean = {
    val self = valueOf(point)

    val neighbourValues = neighbouringPoints(point).map(valueOf)
    neighbourValues.forall(neighbour => neighbour > self)
  }

  lazy val basinPoints: Set[Point] = allPoints.filterNot(point => valueOf(point) == 9)

  def group(basins: Set[Set[Point]], remainder: Set[Point]): Set[Set[Point]] =
    if (remainder.isEmpty) basins
    else {
      val (basin, newRem) = expand(Set(remainder.head), remainder.tail)
      group(basins + basin, newRem)
    }

  def expand(basin: Set[Point], remainder: Set[Point]): (Set[Point], Set[Point]) = {
    val neighbours: Set[Point] = remainder.intersect(basin.flatMap(neighbouringPoints))
    if (neighbours.isEmpty) basin -> remainder
    else expand(basin ++ neighbours, remainder.diff(neighbours))
  }

  def main(args: Array[String]): Unit = {
    val start = System.currentTimeMillis()
    println(s"AOC $year - Day $day")

    val part1 = lowPoints.map(_ + 1).sum
    val mid = System.currentTimeMillis()
    println(s"Answer part 1: $part1 [${mid - start}ms]")

    val part2 = group(Set.empty, basinPoints).map(_.size).toList.sorted.reverse.take(3).product
    val end = System.currentTimeMillis()
    println(s"Answer part 2: $part2 [${end - mid}ms]")
  }
}
