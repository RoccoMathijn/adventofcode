package aoc2022

import util.AocTools
import util.InputGetter.{Example, Live, Mode}

import scala.annotation.tailrec
import scala.collection.mutable

object Day12 extends AocTools(12, 2022) {
//  implicit private val mode: Mode = Example
  implicit private val mode: Mode = Live

  case class Point(x: Int, y: Int)

  val grid: List[List[Int]] = inputLines.map(line => line.toList.map(height))
  val start: Point = findValue('S').head
  val end: Point = findValue('E').head
  val bottomRight: Point = Point(grid.head.size - 1, grid.size - 1)

  def findValue(value: Char): List[Point] =
    inputLines.map(_.toList).zipWithIndex.flatMap {
      case (line, y) => Some(Point(line.indexOf(value), y)).filterNot(_.x == -1)
    }

  def height(item: Char): Int = if (item == 'S') 0 else if (item == 'E') 25 else "abcdefghijklmnopqrstuvwxyz".indexOf(item)

  def heightOf(point: Point): Int = grid(point.y)(point.x)

  def allA: List[Point] = findValue('a')

  def n(point: Point): Set[Point] = {
    val top = Point(point.x, point.y - 1)
    val left = Point(point.x - 1, point.y)
    val down = Point(point.x, point.y + 1)
    val right = Point(point.x + 1, point.y)

    Set(top, left, down, right)
      .filter(point =>
        point.x >= 0 && point.y >= 0 &&
          point.x <= bottomRight.x &&
          point.y <= bottomRight.y
      )
      .filter(n => heightOf(n) - heightOf(point) <= 1)
  }

  def solveFrom(point: Point): Int = {
    d.clear()
    A.clear()
    X.clear()
    d.addOne(point -> 0)
    X.addOne(Distance(point, 0))
    dijkstra(end)
  }

  def solve1: Int = solveFrom(start)
  def solve2: Int = allA.map(solveFrom).min

  case class Distance(point: Point, distance: Int)
  // Map with distance from S per node
  val d: mutable.Map[Point, Int] = mutable.Map.empty
  // Set with visited nodes
  val A: mutable.Set[Point] = mutable.Set.empty[Point]

  val ordering: Ordering[Distance] = Ordering.by(_.distance)
  implicit val minOrdering: Ordering[Distance] = ordering.reverse
  // Reachable nodes from frontier
  val X: mutable.PriorityQueue[Distance] = mutable.PriorityQueue.empty[Distance]

  @tailrec
  def dijkstra(point: Point): Int = {
    if (X.isEmpty) d(point)
    else {
      val x: Point = X.dequeue().point
      A.addOne(x)
      n(x).diff(A).foreach { z: Point =>
        val distance: Int = d(x) + 1
        val dz = d.get(z)
        if (dz.isEmpty || dz.exists(_ > distance)) {
          d.addOne(z -> distance)
          X.addOne(Distance(z, distance))
        }
      }
      dijkstra(point)
    }
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
