package aoc2022

import util.AocTools
import util.InputGetter.{Example, Live, Mode}

object Day04 extends AocTools(4, 2022) {
//      implicit private val mode: Mode = Example
  implicit private val mode: Mode = Live

  def part1 =
    inputLines.count {
      case s"$xmin-$xmax,$ymin-$ymax" =>
        xmin.toInt <= ymin.toInt && xmax.toInt >= ymax.toInt || ymin.toInt <= xmin.toInt && ymax.toInt >= xmax.toInt
    }

  def part2 =
    inputLines.count {
      case s"$xmin-$xmax,$ymin-$ymax" =>
        !(xmin.toInt > ymax.toInt || xmax.toInt < ymin.toInt)
    }

  def main(args: Array[String]): Unit = {
    val start = System.currentTimeMillis()
    println(s"AOC 2022 - Day $day")

    val mid = System.currentTimeMillis()
    println(s"Answer part 1: $part1 [${mid - start}ms]")
    val end = System.currentTimeMillis()
    println(s"Answer part 2: $part2 [${end - mid}ms]")
  }
}
