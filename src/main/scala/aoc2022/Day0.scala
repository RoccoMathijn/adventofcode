package aoc2022

import util.AocTools
import util.InputGetter.{Live, Mode}

object Day0 extends AocTools(0, 2022) {
  //    implicit private val mode: Mode = Example
  implicit private val mode: Mode = Live

  val part1 = ""

  val part2 = ""

  def main(args: Array[String]): Unit = {
    val start = System.currentTimeMillis()
    println(s"AOC 2022 - Day $day")

    val mid = System.currentTimeMillis()
    println(s"Answer part 1: $part1 [${mid - start}ms]")
    val end = System.currentTimeMillis()
    println(s"Answer part 2: $part2 [${end - mid}ms]")
  }
}
