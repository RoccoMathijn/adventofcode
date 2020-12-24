package aoc2020

import util.AocTools
import util.InputGetter.{Live, Mode}

object Day23 extends AocTools(23, 2020) {
  //  implicit private val mode: Mode = Example
  implicit private val mode: Mode = Live

  val input: Seq[String] = inputLines

  def main(args: Array[String]): Unit = {
    val start = System.currentTimeMillis()
    println(s"AOC 2020 - Day $day")

    val part1 = ???
    val mid = System.currentTimeMillis()
    println(s"Answer part 1: $part1 [${mid - start}ms]")

    val part2 = ???
    val end = System.currentTimeMillis()
    println(s"Answer part 2: $part2 [${`end` - mid}ms]")
  }
}
