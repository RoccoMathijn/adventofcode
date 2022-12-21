package aoc2022

import util.AocTools
import util.InputGetter.{Example, Live, Mode}

object Day03 extends AocTools(3, 2022) {
//  implicit private val mode: Mode = Example
  implicit private val mode: Mode = Live

  def priority(item: Char): Int = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ".indexOf(item) + 1

  def part1: Int =
    inputLines
      .flatMap { line =>
        val (left, right) = line.splitAt(line.length / 2)
        left.toSet.intersect(right.toSet)
      }
      .map(priority)
      .sum

  def part2: Int =
    inputLines
      .grouped(3)
      .flatMap {
        case List(one, two, three) =>
          one.toSet
            .intersect(two.toSet)
            .intersect(three.toSet)
            .map(priority)
      }
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
