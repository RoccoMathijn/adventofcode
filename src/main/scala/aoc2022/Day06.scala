package aoc2022

import util.AocTools
import util.InputGetter.{Example, Live, Mode}

object Day06 extends AocTools(6, 2022) {
  implicit private val mode: Mode = Example
//  implicit private val mode: Mode = Live

  val input = inputLines.head

  def packetIndex(packetMarkerSize: Int, dataStreamBuffer: String): Int = {
    val unique = dataStreamBuffer.sliding(packetMarkerSize, 1).find(_.distinct.length == packetMarkerSize).get
    dataStreamBuffer.indexOf(unique) + packetMarkerSize
  }

  def part1: Int = packetIndex(4, input)

  def part2: Int = packetIndex(14, input)

  def main(args: Array[String]): Unit = {
    val start = System.currentTimeMillis()
    println(s"AOC 2022 - Day $day")

    val mid = System.currentTimeMillis()
    println(s"Answer part 1: $part1 [${mid - start}ms]")
    val end = System.currentTimeMillis()
    println(s"Answer part 2: $part2 [${end - mid}ms]")
  }
}
