package aoc2020

import util.AocTools
import util.InputGetter.{Example, Live, Mode}

object Day23 extends AocTools(23, 2020) {
  implicit private val mode: Mode = Example
//  implicit private val mode: Mode = Live

  val input: List[Int] = inputBlob.map(_.toString.toInt).toList
  val part2 = false
  def move(cups: Seq[Int]): Seq[Int] = {
    val x = cups.head
    val pickup = cups.slice(1, 4)
    val xs = cups.drop(4)
    val destinationCup = x - 1
    val index = findLabel(destinationCup, xs, pickup)
    val (front, back) = xs.splitAt(index + 1)
    front ++ pickup ++ back :+ x
  }

  def findLabel(destinationCupValue: Int, cups: Seq[Int], pickup: Seq[Int]): Int = {
    if (pickup.contains(destinationCupValue)) findLabel(destinationCupValue - 1, cups, pickup)
    else if (destinationCupValue < 1) {
      if (part2) findLabel(1000000, cups, pickup) else findLabel(input.max, cups, pickup)
    } else {
      cups.indexOf(destinationCupValue)
    }
  }

  def moveN(cups: Seq[Int])(n: Int): Seq[Int] =
    (1 to n).foldLeft(cups) { (movedCups, n) =>
      println(s"-- move $n --")
      move(movedCups)
    }

  def answer(cups: Seq[Int]): String =
    cups match {
      case 1 :: xs => xs.mkString
      case x :: xs => answer(xs :+ x)
    }

  def answer2(cups: Seq[Int]): Long = {
    val indexOfOne = cups.indexOf(1)
    val oneAfter = cups(indexOfOne + 1).toLong
    val twoAfter = cups(indexOfOne + 2).toLong
    println(oneAfter)
    println(twoAfter)
    oneAfter * twoAfter
  }

  val startingList = input.toVector ++ ((input.max + 1) to 1000000).toVector

  def main(args: Array[String]): Unit = {
    val start = System.currentTimeMillis()
    println(s"AOC 2020 - Day $day")

    val part1 = answer(moveN(input)(100))
    val mid = System.currentTimeMillis()
    println(s"Answer part 1: $part1 [${mid - start}ms]")

    val part2 = answer2(moveN(startingList)(10000000))
    val end = System.currentTimeMillis()
    println(s"Answer part 2: $part2 [${`end` - mid}ms]")
  }
}
