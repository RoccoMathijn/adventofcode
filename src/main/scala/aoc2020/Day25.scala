package aoc2020

import util.AocTools
import util.InputGetter.{Example, Live, Mode}

import scala.annotation.tailrec

object Day25 extends AocTools(25, 2020) {
//  implicit private val mode: Mode = Example
  implicit private val mode: Mode = Live

  val input: Seq[Int] = inputLines.map(_.toInt)
  val cardPubKey = input(0)
  val doorPubKey = input(1)

  val cardLoopsize = findLoopSize(cardPubKey, passthroughValue = 1L)
  val doorLoopsize = findLoopSize(doorPubKey, passthroughValue = 1L)

  def transform(loopSize: Int, subject: Long, value: Long): Long = {
    (1 to loopSize).foldLeft(value) { (newValue, _) => (newValue * subject) % 20201227 }
  }

  def findLoopSize(pubKey: Int, loopSize: Int = 0, passthroughValue: Long): Long = {
    val initialValue = transform(loopSize = 1, subject = 7, value = passthroughValue)
    if (pubKey == initialValue) loopSize + 1
    else {
      findLoopSize(pubKey, loopSize + 1, initialValue)
    }
  }

  def main(args: Array[String]): Unit = {
    val start = System.currentTimeMillis()
    println(s"AOC 2020 - Day $day")

    val cardSecret = transform(doorLoopsize.toInt, cardPubKey, 1)
    val part1 = cardSecret
    val mid = System.currentTimeMillis()
    println(s"Answer part 1: $part1 [${mid - start}ms]")
  }
}
