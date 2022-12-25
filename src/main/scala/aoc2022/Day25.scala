package aoc2022

import util.AocTools
import util.InputGetter._

object Day25 extends AocTools(25, 2022) {
//  implicit private val mode: Mode = Example
  implicit private val mode: Mode = Live

  val input = inputLines

  def fromSnafu(string: String): Long = {
    string.reverse.zipWithIndex.map {
      case (c, i) =>
        snafu2IntMap(c) * math.pow(5, i).toLong
    }
  }.sum

  def toBaseFive(integer: Long, acc: List[Int] = List.empty): List[Int] = {
    if (integer < 5) integer.toInt :: acc
    else toBaseFive(integer / 5, (integer % 5).toInt :: acc)
  }

  val snafu2IntMap =
    Map(
      '2' -> 2,
      '1' -> 1,
      '0' -> 0,
      '-' -> -1,
      '=' -> -2
    )
  val int2SnafuMap = snafu2IntMap.toList.map(_.swap).toMap

  def fromBase5ToSnafu(baseFive: List[Int]): String = {
    val (res, carry) = {
      baseFive.reverse.foldLeft(List.empty[Char] -> 0) {
        case ((acc, carry), i) =>
          val x = carry + i
          if (x > 2) (int2SnafuMap(x - 5) :: acc) -> 1
          else (int2SnafuMap(x) :: acc) -> 0
      }
    }
    if (carry > 0) (carry :: res).mkString else res.mkString
  }

  def solve1: String = fromBase5ToSnafu(toBaseFive(inputLines.map(fromSnafu).sum))
  def solve2 = ???

  def main(args: Array[String]): Unit = {
    val start = System.currentTimeMillis()
    println(s"AOC 2022 - Day $day")

    val part1 = solve1
    val mid = System.currentTimeMillis()
    println(s"Answer part 1: $part1 [${mid - start}ms]")
    val part2 = solve2
    val end = System.currentTimeMillis()
    println(s"Answer part 2: $part2 [${end - mid}ms]")
  }
}
