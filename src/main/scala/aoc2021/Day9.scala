package aoc2021

import util.AocTools
import util.InputGetter.{Example, Live, Mode}

import scala.util.Try

object Day9 extends AocTools(9, 2021) {
//  implicit private val mode: Mode = Example
  implicit private val mode: Mode = Live

  lazy val input: List[List[Int]] = inputLines.map(_.toList.map(_.toString.toInt))

  lazy val lowPoints: List[Int] = allPoints.toList.collect { case point if isLowPoint(point.x, point.y) => input(point.y)(point.x) }
  
  def isLowPoint(x: Int, y: Int) = {
    val self = input(y)(x)

    val up = Try(input(y)(x - 1))
    val down = Try(input(y + 1)(x))
    val left = Try(input(y - 1)(x))
    val right = Try(input(y)(x + 1))

    val neighbours = List(up, down, left, right).filter(_.isSuccess).map(_.get)
    neighbours.forall(neighbour => neighbour > self)
  }

  case class Point(x: Int, y: Int)

  lazy val allPoints: Set[Point] = input.head.indices.flatMap(x => input.indices.map(y => Point(x, y))).toSet

  lazy val basinPoints: Set[Point] = allPoints.filterNot(point => input(point.y)(point.x) == 9)

  def group(basins: Set[Set[Point]], remainder: Set[Point]): Set[Set[Point]] =
    remainder match {
      case s if s.isEmpty => basins
      case s =>
        val (basin, newRem) = expand(Set(s.head), s.tail)
        group(basins + basin, newRem)
    }

  def expand(basin: Set[Point], remainder: Set[Point]): (Set[Point], Set[Point]) = {
    val neighbours: Set[Point] = basin.flatMap(basinPoint => remainder.filter(remainderPoint => isNeighbour(basinPoint, remainderPoint)))
    if (neighbours.isEmpty) basin -> remainder
    else expand(basin ++ neighbours, remainder.diff(neighbours))
  }

  def isNeighbour(point: Point, point2: Point): Boolean =
    point -> point2 match {
      case (Point(x1, y1), Point(x2, y2)) =>
        x1 == x2 && math.abs(y1 - y2) == 1 || math.abs(x1 - x2) == 1 && y1 == y2
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
