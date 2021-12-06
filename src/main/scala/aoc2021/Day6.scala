package aoc2021

import util.AocTools
import util.InputGetter.{Example, Live, Mode}

object Day6 extends AocTools(6, 2021) {
//  implicit private val mode: Mode = Example
  implicit private val mode: Mode = Live

  val input: Map[Int, Long] = inputLines.head.split(',').map(_.toInt).toList.groupBy(identity).view.mapValues(_.size.toLong).toMap
  
  def step(school: Map[Int, Long]): Map[Int, Long] =
    school.toList.flatMap { case (age, n) => if (age - 1 < 0) List(6 -> n, 8 -> n) else List((age - 1) -> n) }.groupBy(_._1).view.mapValues(_.map(_._2).sum).toMap

  def solve(n: Int): Long = (1 to n).foldLeft(input)((gen, _) => step(gen)).values.sum

  def main(args: Array[String]): Unit = {
    val start = System.currentTimeMillis()
    println(s"AOC $year - Day $day")

    val part1 = solve(80)
    val mid = System.currentTimeMillis()
    println(s"Answer part 1: $part1 [${mid - start}ms]")

    val part2 = solve(256)
    val end = System.currentTimeMillis()
    println(s"Answer part 2: $part2 [${end - mid}ms]")
  }
}
