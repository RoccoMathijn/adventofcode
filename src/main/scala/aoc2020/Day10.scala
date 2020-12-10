package aoc2020

import util.AocTools
import util.InputGetter.{Live, Mode}

import scala.collection.mutable

object Day10 extends AocTools(10, 2020) {
//  implicit private val mode: Mode = Example
  implicit private val mode: Mode = Live

  val input = inputInts

  val start = 0
  val end = input.max + 3

  val sorted = (inputInts :+ start :+ end).sorted

  def joltageDiff(input: Seq[Int]): Int = {
    val diffs = input.sorted.sliding(2).map { case List(a, b) => b - a }.toList
    val ones = diffs.count(x => x == 1) + 1
    val threes = diffs.count(x => x == 3) + 1
    ones * threes
  }

  val reachMap: Map[Int, Seq[Int]] = sorted.map { i =>
    val reachables = sorted.dropWhile(n => n < i).takeWhile(n => n <= i + 3).drop(1)
    i -> reachables
  }.toMap

  val cache: mutable.Map[Int, Long] = mutable.Map.empty[Int, Long]
  def arrangements(input: Int): Long = {
    if (cache.contains(input)) cache(input)
    else {
      val ans = reachMap(input).map { i =>
        if (i == end) 1
        else arrangements(i)
      }.sum
      cache.addOne(input -> ans)
      ans
    }
  }

  def main(args: Array[String]): Unit = {
    println(s"AOC 2020 - Day $day")

    val part1 = joltageDiff(input)
    println(s"Answer part 1: $part1")

    val part2 = arrangements(start)
    println(s"Answer part 2: $part2")
  }
}
