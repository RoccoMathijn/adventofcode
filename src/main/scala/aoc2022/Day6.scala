package aoc2022

import util.AocTools
import util.InputGetter.{Example, Live, Mode}

object Day6 extends AocTools(6, 2022) {
//  implicit private val mode: Mode = Example
  implicit private val mode: Mode = Live

  def packetMarker(uniqueSize: Int) = {
    val line = inputLines.head
    val unique = line.sliding(uniqueSize, 1).toList.find(group => group.distinct.size == uniqueSize).get
    line.indexOf(unique) + uniqueSize
  }

  def part1 = packetMarker(4)

  def part2 = packetMarker(14)

  def main(args: Array[String]): Unit = {
    val start = System.currentTimeMillis()
    println(s"AOC 2022 - Day $day")

    val mid = System.currentTimeMillis()
    println(s"Answer part 1: $part1 [${mid - start}ms]")
    val end = System.currentTimeMillis()
    println(s"Answer part 2: $part2 [${end - mid}ms]")
  }
}
