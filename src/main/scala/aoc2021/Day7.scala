package aoc2021

import util.AocTools
import util.InputGetter.{Example, Live, Mode}

object Day7 extends AocTools(7, 2021) {
//  implicit private val mode: Mode = Example
  implicit private val mode: Mode = Live

  val crabs: Seq[Int] = inputLines.head.split(',').map(_.toInt).toList

  def answer: Int = (1 to crabs.max).map(i => crabs.map(crab => math.abs(i - crab)).sum).min

  def answer2: Int = (1 to crabs.max)
    .map(i =>
      crabs.map { crab =>
        val n = math.abs(i - crab)
        n * (n + 1) / 2
      }.sum
    )
    .min

  def main(args: Array[String]): Unit = {
    val start = System.currentTimeMillis()
    println(s"AOC $year - Day $day")

    val part1 = answer
    val mid = System.currentTimeMillis()
    println(s"Answer part 1: $part1 [${mid - start}ms]")

    val part2 = answer2
    val end = System.currentTimeMillis()
    println(s"Answer part 2: $part2 [${end - mid}ms]")
  }
}
