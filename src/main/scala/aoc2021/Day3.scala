package aoc2021

import util.AocTools
import util.InputGetter.{Example, Live, Mode}

object Day3 extends AocTools(3, 2021) {
//      implicit private val mode: Mode = Example
  implicit private val mode: Mode = Live

  def gamma: String = inputLines.transpose.map { col =>
    val ones = col.count(_ == '1')
    val zeros = col.count(_ == '0')

    if (ones > zeros) '1'
    else '0'
  }.mkString

  def epsilon: String = inputLines.transpose.map { col =>
    val ones = col.count(_ == '1')
    val zeros = col.count(_ == '0')

    if (ones < zeros) '1'
    else '0'
  }.mkString

  def mostCommon(diagnostics: List[String], pos: Int): Char = {
    val col = diagnostics.map(_(pos))
    val ones = col.count(_ == '1')
    val zeros = col.count(_ == '0')

    if (ones >= zeros) '1'
    else '0'
  }

  def leastCommon(diagnostics: List[String], pos: Int): Char = {
    val col = diagnostics.map(_(pos))
    val ones = col.count(_ == '1')
    val zeros = col.count(_ == '0')

    if (ones < zeros) '1'
    else '0'
  }

  def oxygenGenerator(diagnostics: List[String], pos: Int): String = {
    if (diagnostics.size == 1) diagnostics.head
    else {
      oxygenGenerator(diagnostics.filter(_(pos) == mostCommon(diagnostics, pos)), pos + 1)
    }
  }

  def co2Scrubber(diagnostics: List[String], pos: Int): String = {
    if (diagnostics.size == 1) diagnostics.head
    else {
      co2Scrubber(diagnostics.filter(_(pos) == leastCommon(diagnostics, pos)), pos + 1)
    }
  }
  
  def main(args: Array[String]): Unit = {
    val start = System.currentTimeMillis()
    println(s"AOC 2021 - Day $day")

    val part1 = Integer.parseInt(gamma.mkString, 2) * Integer.parseInt(epsilon.mkString, 2)
    val mid = System.currentTimeMillis()
    println(s"Answer part 1: $part1 [${mid - start}ms]")
    
    val oxygenGeneratorRating = oxygenGenerator(inputLines, 0)
    val co2ScrubberRating = co2Scrubber(inputLines, 0)
    val oxygenDec: Int = Integer.parseInt(oxygenGeneratorRating, 2)
    val co2ScrubberDec: Int = Integer.parseInt(co2ScrubberRating, 2)
    val part2 = oxygenDec * co2ScrubberDec
    
    val end = System.currentTimeMillis()
    println(s"Answer part 2: $part2 [${end - mid}ms]")
  }
}
