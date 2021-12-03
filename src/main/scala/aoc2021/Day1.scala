package aoc2021

import util.AocTools
import util.InputGetter.{Live, Mode}

object Day1 extends AocTools(1, 2021) {
//    implicit private val mode: Mode = Example
  implicit private val mode: Mode = Live

  val part1 = inputInts.sliding(2).foldLeft(0)((acc, group) => if (group.head < group(1)) acc + 1 else acc)

  val part2 = inputInts.sliding(3).map(_.sum).sliding(2).foldLeft(0)((acc, group) => if (group.head < group(1)) acc + 1 else acc)

  def main(args: Array[String]): Unit = {
    val start = System.currentTimeMillis()
    println(s"AOC 2021 - Day $day")

    val mid = System.currentTimeMillis()
    println(s"Answer part 1: $part1 [${mid - start}ms]")
    val end = System.currentTimeMillis()
    println(s"Answer part 2: $part2 [${end - mid}ms]")
  }
}
