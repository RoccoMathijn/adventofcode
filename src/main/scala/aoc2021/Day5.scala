package aoc2021

import util.AocTools
import util.InputGetter.{Example, Live, Mode}

import scala.util.matching.Regex

object Day5 extends AocTools(5, 2021) {
//  implicit private val mode: Mode = Example
  implicit private val mode: Mode = Live

  val Vent: Regex = "(\\d+),(\\d+) -> (\\d+),(\\d+)".r
  case class Coordinate(x: Int, y: Int)
  def toRange(x: Int, y: Int) = {
    val range = math.min(x, y) to math.max(x, y)

    if (x < y) range
    else range.reverse
  }

  val vents: List[(Coordinate, Coordinate)] = inputLines.map { case Vent(x1, y1, x2, y2) => Coordinate(x1.toInt, y1.toInt) -> Coordinate(x2.toInt, y2.toInt) }
  
  val horizontalAndVertical: Seq[(Int, Int)] = vents.collect {
    case Coordinate(x1, y1) -> Coordinate(x2, y2) if x1 == x2 => toRange(y1, y2).map(y => x1 -> y)
    case Coordinate(x1, y1) -> Coordinate(x2, y2) if y1 == y2 => toRange(x1, x2).map(x => x -> y1)
  }.flatten

  val diagonals: Seq[(Int, Int)] = vents.collect {
    case Coordinate(x1, y1) -> Coordinate(x2, y2) if x1 != x2 && y1 != y2 => toRange(x1, x2).zip(toRange(y1, y2))
  }.flatten

  def countOverlapping(lines: Seq[(Int, Int)]): Int = lines.groupBy(identity).values.count(_.size >= 2)

  def main(args: Array[String]): Unit = {
    val start = System.currentTimeMillis()
    println(s"AOC $year - Day $day")

    val part1 = countOverlapping(horizontalAndVertical)
    val mid = System.currentTimeMillis()
    println(s"Answer part 1: $part1 [${mid - start}ms]")

    val part2 = countOverlapping(horizontalAndVertical ++ diagonals)
    val end = System.currentTimeMillis()
    println(s"Answer part 2: $part2 [${end - mid}ms]")
  }
}
