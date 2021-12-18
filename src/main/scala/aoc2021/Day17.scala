package aoc2021

import util.AocTools
import util.InputGetter.{Live, Mode}

object Day17 extends AocTools(17, 2021) {
//  implicit private val mode: Mode = Example
  implicit private val mode: Mode = Live

  case class Point(x: Int, y: Int)
  case class TargetArea(from: Point, to: Point)
  val targetArea: TargetArea = inputLines.head match {
    case s"target area: x=$x..$x2, y=$y2..$y" => TargetArea(Point(x.toInt, y.toInt), Point(x2.toInt, y2.toInt))
  }
  println(targetArea)

  def withinTarget(point: Point): Boolean = point.x >= targetArea.from.x && point.y <= targetArea.from.y && point.x <= targetArea.to.x && point.y >= targetArea.to.y

  def launch(xV: Int, yV: Int, point: Point = Point(0, 0), maxHeight: Int = 0): Option[Int] = {
    if (withinTarget(point)) Some(maxHeight)
    else if (point.x > targetArea.to.x || point.y < targetArea.to.y) None
    else {
      val newPosition = Point(xV + point.x, point.y + yV)
      val newXv = if (xV > 0) xV - 1 else if (xV < 0) xV + 1 else 0
      val newYv = yV - 1
      val newHeight = math.max(maxHeight, point.y)
      launch(newXv, newYv, newPosition, newHeight)
    }
  }

  def solve1: Int =
    (for {
      x <- 1 to targetArea.to.x
      y <- 0 to 250
      res = launch(x, y)
      if (res.isDefined)
    } yield res.get).max

  def solve2: Int =
    (for {
      x <- 1 to targetArea.to.x
      y <- targetArea.to.y to 250
      res = launch(x, y)
      if (res.isDefined)
    } yield res).size

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
