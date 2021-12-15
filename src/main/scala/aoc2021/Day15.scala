package aoc2021

import util.AocTools
import util.InputGetter.{Live, Mode}

import scala.collection.mutable

object Day15 extends AocTools(15, 2021) {
//  implicit private val mode: Mode = Example
  implicit private val mode: Mode = Live

  case class Point(x: Int, y: Int)
  val grid: List[List[Int]] = inputLines.map(_.map(_.toString.toInt).toList)
  val pointZero: Point = Point(0, 0)

  def valueOf(point: Point): Int = {
    val gridNumber = point.x / grid.head.size + point.y / grid.size
    val original = grid(point.y % grid.size)(point.x % grid.head.size)
    val supergrid = original + gridNumber
    if (supergrid > 9) supergrid - 9
    else supergrid
  }

  val bottomRight = Point(grid.head.size - 1, grid.size - 1)
  val bottomRightSuperGrid = Point(grid.head.size * 5 - 1, grid.size * 5 - 1)

  def n(point: Point, max: Point): Set[Point] = {
    val top = Point(point.x, point.y - 1)
    val left = Point(point.x - 1, point.y)
    val down = Point(point.x, point.y + 1)
    val right = Point(point.x + 1, point.y)

    Set(top, left, down, right).filter(point => point.x >= 0 && point.y >= 0 && point.x <= max.x && point.y <= max.y)
  }

  // distance from (0,0)
  val d: mutable.Map[Point, Int] = {
    val i = mutable.Map[Point, Int](pointZero -> 0)
    n(pointZero, bottomRight).foreach(n => i.update(n, valueOf(n)))
    i
  }
  val A: mutable.Set[Point] = mutable.Set.empty[Point]
  val X: mutable.Set[Point] = mutable.Set.empty[Point]

  def dijkstra(point: Point): Int = {
    if (X.isEmpty) d(point)
    else {
      val x: Point = X.minBy(d)
      X.remove(x)
      A.addOne(x)
      n(x, point).diff(A).foreach { z: Point =>
        val distance: Int = d(x) + valueOf(z)
        if (X.contains(z)) {
          d.updateWith(z)(_.map(existing => math.min(existing, distance)).orElse(Some(distance)))
        } else {
          X.addOne(z)
          d.update(z, distance)
        }
      }
      dijkstra(point)
    }
  }

  def solve1: Int = {
    A.clear()
    A.addOne(pointZero)
    X.clear()
    X.addAll(n(pointZero, bottomRight)).diff(A)
    A.clear()
    A.addOne(pointZero)
    dijkstra(bottomRight)
  }

  def solve2: Int = {
    A.clear()
    A.addOne(pointZero)
    X.clear()
    X.addAll(n(pointZero, bottomRightSuperGrid)).diff(A)
    d.clear()
    d.addOne(pointZero -> 0)
    n(pointZero, bottomRight).foreach(n => d.update(n, valueOf(n)))
    dijkstra(bottomRightSuperGrid)
  }

  def main(args: Array[String]): Unit = {
    val start = System.currentTimeMillis()
    println(s"AOC $year - Day $day")

    val part1 = solve1

    val mid = System.currentTimeMillis()
    println(s"Answer part 1: $part1 [${mid - start}ms]")

    val part2 = solve2
    val end = System.currentTimeMillis()
    println(s"Answer part 2: $part2 [${end - mid}ms]")
  }
}
