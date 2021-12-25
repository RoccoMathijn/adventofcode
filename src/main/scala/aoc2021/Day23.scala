package aoc2021

import util.AocTools
import util.InputGetter.{Live, Mode}

object Day23 extends AocTools(23, 2021) {
//  implicit private val mode: Mode = Example
  implicit private val mode: Mode = Live

  val A = 1
  val B = 10
  val C = 100
  val D = 1000

  def solve1 = D * 2 + B * 4 + C * 5 + D * 6 + D * 2 + C * 3 + C * 4 + A * 3 + B * 3 + A * 2 + B * 5 + A * 3 + A * 4
  def solve2 =
    C * 6 + C * 5 + B * 4 + A * 8 + B * 5 + B * 6 + B * 6 + A * 8 + D * 7 + C * 5 + C * 6 + D * 4 + A * 4 + C * 7 + C * 7 + D * 7 + D * 8 + A * 8 + D * 10 + D * 10 + B * 7 + A * 5 + A * 5 + A * 9 + A * 9

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
