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
  }
  
  def noBeacon(pair: (Point, Point), row: Int): Set[Point] = {
    val distance = manhattanDistance(pair._1, pair._2)

    val distanceToRow = math.abs(pair._1.y - row)
    if (distanceToRow > distance) Set.empty
    else {

      (for {
        x <- -math.abs(distanceToRow - distance) to math.abs(distanceToRow - distance)
      } yield Point(pair._1.x + x, row)).toSet
    }
  }

  case class Interval(from: Int, to: Int)
  def check(sensors: List[(Point, Point)], row: Int, ranges: List[Interval], maxX: Int): List[Interval] = {
//    println(ranges)
    if (sensors.isEmpty) ranges
    else {
      val sensorPair = sensors.head
      val mh = manhattanDistance(sensorPair._1, sensorPair._2)
//      println(sensorPair)
//      println(mh)
      val distanceToRow = math.abs(sensorPair._1.y - row)
//      println(distanceToRow)
      if (distanceToRow > mh) check(sensors.tail, row, ranges, maxX)
      else {
        val minXRelative = -(mh - distanceToRow)
        val maxXRel = mh - distanceToRow
//        println(s"MinXRel=$minXRelative, MaxXRel=$maxXRel")
        val rangeOnRow = Interval(bound(minXRelative + sensorPair._1.x, maxX), bound(maxXRel + sensorPair._1.x, maxX))
//        println(rangeOnRow)
        check(sensors.tail, row, rangeOnRow :: ranges, maxX)
      }
    }
  }

  def bound(i: Int, max: Int): Int = {
    math.min(math.max(0, i), max)
  }

  def mergeIntervalList(intervals: List[Interval]): List[Interval] = {
//    println(s"merging $intervals")
    val res = intervals.tail.foldLeft(List(intervals.head))((acc, i) => mergeIntervals(acc.head,i) ++ acc).distinct.sortBy(_.from)
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

  def checkRow(y: Int, bound: Int): Point = {
    if (y > bound) Point(0, 0)
    else {
      mergeIntervalList(check(input, y, List.empty, bound)) match {
        case List(_) =>
          checkRow(y + 1, bound)
        case List(i1, ) =>
          Point(i1.to+1, y)
      }
    }
  }

  def manhattanDistance(point1: Point, point2: Point): Int = {
    Math.abs(point1.x - point2.x) + Math.abs(point1.y - point2.y)
  }

  def tuningFrequency(point: Point): Long = point.x.toLong * 4000000 + point.y.toLong

  def solve1: Long = input.toSet.flatMap(noBeacon(_, 2000000)).diff(input.map(_._1).toSet ++ input.map(_._2).toSet).size

  def solve2: Long = tuningFrequency(checkRow(0, 4000000))

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
