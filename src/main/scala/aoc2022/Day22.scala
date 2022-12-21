package aoc2022

import util.AocTools
import util.InputGetter._

object Day22 extends AocTools(22, 2022) {
//  implicit private val mode: Mode = Example
  implicit private val mode: Mode = Live
  val input = inputLines

  def solve1 = ???
  def solve2 = ???

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
