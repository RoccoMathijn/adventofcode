package aoc2022

import util.AocTools
import util.InputGetter.{Example, Live, Mode}

object Day3 extends AocTools(3, 2022) {
//  implicit private val mode: Mode = Example
  implicit private val mode: Mode = Live

  def rucksacks: List[Char] =
    inputLines.flatMap { line =>
      val (left, right) = line.splitAt(line.length / 2)

      line.find(c => left.contains(c) && right.contains(c))
    }

  val priorities = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
  def part1: Int = rucksacks.map(x => priorities.indexOf(x) + 1).sum

  def part2: Int =
    inputLines
      .grouped(3)
      .flatMap(group =>
        group.head
          .find(c => group(0).contains(c) && group(1).contains(c) && group(2).contains(c))
          .map(c => priorities.indexOf(c) + 1)
      )
      .sum

  def main(args: Array[String]): Unit = {
    val start = System.currentTimeMillis()
    println(s"AOC 2022 - Day $day")

    val mid = System.currentTimeMillis()
    println(s"Answer part 1: $part1 [${mid - start}ms]")
    val end = System.currentTimeMillis()
    println(s"Answer part 2: $part2 [${end - mid}ms]")
  }
}
