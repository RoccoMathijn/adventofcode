package aoc2022

import util.AocTools
import util.InputGetter.{Example, Live, Mode}

object Day6 extends AocTools(6, 2022) {
//  implicit private val mode: Mode = Example
  implicit private val mode: Mode = Live

  def input(size: Int) = {
    val line = inputLines.head
    val unique = line.sliding(size, 1).toList.find(group => group.distinct.size == size).get
    line.indexOf(unique) + size
  }

  def part1 = input(4)

  def part2 = input(14)

  def main(args: Array[String]): Unit = {
    val start = System.currentTimeMillis()
    println(s"AOC 2022 - Day $day")

    val mid = System.currentTimeMillis()
    println(s"Answer part 1: $part1 [${mid - start}ms]")
    val end = System.currentTimeMillis()
    println(s"Answer part 2: $part2 [${end - mid}ms]")
  }
}
