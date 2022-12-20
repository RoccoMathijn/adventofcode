package aoc2022

import aoc2022.Day20Helper.{mixN, nthNumberAfterZero}
import util.AocTools
import util.InputGetter.{Example, Live, Mode}

object Day20 extends AocTools(20, 2022) {
//  implicit private val mode: Mode = Example
  implicit private val mode: Mode = Live

  val input: List[(Long, Int)] = inputLongs.zipWithIndex

  def solve1: Long = {
    val newList = mixN(input, 1).map(_._1)
    val one = nthNumberAfterZero(1000, newList)
    val two = nthNumberAfterZero(2000, newList)
    val three = nthNumberAfterZero(3000, newList)

    println(s"One:$one,two:$two,$three")
    one + two + three
  }

  def solve2: Long = {
    val decryptionKey = 811589153L
    val mapped: List[(Long, Int)] = input.map(x => x._1 * decryptionKey -> x._2)
    val newList = mixN(mapped, 10).map(_._1)
    val one = nthNumberAfterZero(1000, newList)
    val two = nthNumberAfterZero(2000, newList)
    val three = nthNumberAfterZero(3000, newList)

    println(s"One:$one,two:$two,$three")
    one + two + three
  }

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

object Day20Helper {
  def moveIndexByPositions(i: Int, positions: Long, list: List[(Long, Int)]): List[(Long, Int)] = {
    val size = list.size
    val newPosition = wrapAround(i + positions, size - 1)
    val filtered = list.take(i) ++ list.drop(i + 1)
    val (left, right) = filtered.splitAt(newPosition)
    (left :+ list(i)) ++ right
  }

  def wrapAround(i: Long, size: Int): Int = {
    val offset = (i % size).toInt
    if (offset < 0) offset + size else offset
  }

  def nthNumberAfterZero(n: Int, list: List[Long]): Long = {
    val index = list.indexOf(0)
    list((index + n) % list.size)
  }

  def mix(listToMix: List[(Long, Int)]): List[(Long, Int)] = {
    listToMix.indices.foldLeft(listToMix) { (acc, n) =>
      val ((originalNumber, _), newIndex) = acc.zipWithIndex.find(_._1._2 == n).get
      Day20Helper.moveIndexByPositions(newIndex, originalNumber, acc)
    }
  }

  def mixN(input: List[(Long, Int)], rounds: Int): List[(Long, Int)] = {
    if (rounds == 0) input
    else {
      println(s"Round: $rounds")
      mixN(mix(input), rounds - 1)
    }
  }
}
