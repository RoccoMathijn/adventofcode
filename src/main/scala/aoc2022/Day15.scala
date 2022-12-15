package aoc2022

import aoc2022.Prelude.Point
import util.AocTools
import util.InputGetter.{Example, Live, Mode}

object Day15 extends AocTools(15, 2022) {
//  implicit private val mode: Mode = Example
  implicit private val mode: Mode = Live
  val input = inputLines.map {
    case s"Sensor at x=$sx, y=$sy: closest beacon is at x=$bx, y=$by" =>
      Point(sx.toInt, sy.toInt) -> Point(bx.toInt, by.toInt)
  }.toSet

  case class Interval(from: Int, to: Int)
  def coverageOnRow(sensors: Set[(Point, Point)], row: Int, ranges: Set[Interval], minX: Int, maxX: Int): Set[Interval] = {
    if (sensors.isEmpty) ranges
    else {
      val sensorPair = sensors.head
      val mh = manhattanDistance(sensorPair._1, sensorPair._2)
      val distanceToRow = math.abs(sensorPair._1.y - row)
      if (distanceToRow > mh) coverageOnRow(sensors.tail, row, ranges, minX, maxX)
      else {
        val minXRelative = -(mh - distanceToRow)
        val maxXRel = mh - distanceToRow
        val rangeOnRow = Interval(bound(minXRelative + sensorPair._1.x, minX, maxX), bound(maxXRel + sensorPair._1.x, minX, maxX))
        coverageOnRow(sensors.tail, row, ranges+rangeOnRow , minX, maxX)
      }
    }
  }

  def bound(i: Int, min: Int, max: Int): Int = {
    math.min(math.max(min, i), max)
  }

  def mergeIntervalList(intervals: List[Interval]): List[Interval] = {
    val res = intervals.tail.foldLeft(List(intervals.head))((acc, i) => mergeIntervals(acc.head, i) ++ acc).distinct.sortBy(_.from)
    if (res == intervals) res
    else mergeIntervalList(res)
  }

  def mergeIntervals(left: Interval, right: Interval): List[Interval] = {
    if (overlap(left, right)) List(Interval(math.min(left.from, right.from), math.max(left.to, right.to)))
    else {
      List(right, left)
    }
  }

  def overlap(left: Interval, right: Interval): Boolean = {
    !(left.from > right.to || left.to < right.from)
  }

  def checkAllRows(from: Int, to: Int): Point = {
    if (from > to) Point(0, 0)
    else {
      mergeIntervalList(coverageOnRow(input, from, Set.empty, 0, to).toList) match {
        case List(_) =>
          checkAllRows(from + 1, to)
        case List(i1, _) =>
          Point(i1.to + 1, from)
      }
    }
  }

  def manhattanDistance(point1: Point, point2: Point): Int = {
    Math.abs(point1.x - point2.x) + Math.abs(point1.y - point2.y)
  }

  def tuningFrequency(point: Point): Long = point.x.toLong * 4000000 + point.y.toLong

  def solve1: Long = {
//    val row = 10
    val row = 2000000
    coverageOnRow(input, row, Set.empty, Int.MinValue, Int.MaxValue)
      .flatMap { case Interval(from, to) => from to to}
      .diff(input.filter(_._1.y == row).map(_._1.x) ++ input.filter(_._2.y == row).map(_._2.x))
      .size
  }

  def solve2: Long = {
    val point = checkAllRows(0, 4000000) 
    tuningFrequency(point)
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
