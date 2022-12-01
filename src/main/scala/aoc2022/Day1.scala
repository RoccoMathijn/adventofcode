package aoc2022

import util.AocTools
import util.InputGetter.{Example, Live, Mode}

object Day1 extends AocTools(1, 2022) {
//    implicit private val mode: Mode = Example
  implicit private val mode: Mode = Live

  private val sumByElves: Array[Int] = inputBlob.split("\n\n").map(_.split("\n").map(_.toInt).sum)

  val part1 = sumByElves.max

  val part2 = sumByElves.sorted.reverse.take(3).sum

  def main(args: Array[String]): Unit = {
    val start = System.currentTimeMillis()
    println(s"AOC 2022 - Day $day")

    val mid = System.currentTimeMillis()
    println(s"Answer part 1: $part1 [${mid - start}ms]")
    val end = System.currentTimeMillis()
    println(s"Answer part 2: $part2 [${end - mid}ms]")
  }
}
