package aoc2021

import util.AocTools
import util.InputGetter.{Live, Mode}

object Day8 extends AocTools(8, 2021) {
//  implicit private val mode: Mode = Example
  implicit private val mode: Mode = Live

  def toNumber(output: List[String], nMap: Map[String, Int]): Int = output
      .map(outputDigit => nMap.find(entry => entry._1.length == outputDigit.length && entry._1.forall(char => outputDigit.contains(char))).get._2)
      .mkString
      .toInt

  case class Entry(signals: List[String], output: List[String])

  val input: Seq[Entry] = inputLines.map { line: String =>
    val List(signals, output) = line.split('|').map(_.trim.split(' ').toList).toList
    Entry(signals, output)
  }

  def deduct(signals: List[String]): Map[String, Int] = {
    val one: String = signals.find(_.length == 2).get
    val four: String = signals.find(_.length == 4).get
    val seven: String = signals.find(_.length == 3).get
    val eighth: String = signals.find(_.length == 7).get

    val nine: String = signals.filter(_.length == 6).filter(nine => four.forall(c => nine.contains(c))).head
    val zero: String = signals.filter(_.length == 6).filterNot(_ == nine).filter(zero => one.forall(c => zero.contains(c))).head
    val six: String = signals.filter(_.length == 6).filterNot(_ == nine).filterNot(_ == zero).head

    val three: String = signals.filter(_.length == 5).filter(three => seven.forall(c => three.contains(c))).head

    val e: Char = eighth.diff(nine).head
    val two: String = signals.filter(_.length == 5).filterNot(_ == three).filter(_.contains(e)).head
    val five: String = signals.filter(_.length == 5).filterNot(_ == three).filterNot(_ == two).head

    Map(
      zero -> 0,
      one -> 1,
      two -> 2,
      three -> 3,
      four -> 4,
      five -> 5,
      six -> 6,
      seven -> 7,
      eighth -> 8,
      nine -> 9
    )
  }

  def main(args: Array[String]): Unit = {
    val start = System.currentTimeMillis()
    println(s"AOC $year - Day $day")

    val part1 = input.map { entry =>
      entry.output.count(string => string.length == 2 || string.length == 4 || string.length == 3 || string.length == 7)
    }.sum
    val mid = System.currentTimeMillis()
    println(s"Answer part 1: $part1 [${mid - start}ms]")

    val part2 = input.map(entry => toNumber(entry.output, deduct(entry.signals))).sum
    val end = System.currentTimeMillis()
    println(s"Answer part 2: $part2 [${end - mid}ms]")
  }
}
