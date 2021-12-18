package aoc2021

import util.AocTools
import util.InputGetter.{Live, Mode}

object Day18 extends AocTools(18, 2021) {
//  implicit private val mode: Mode = Example
  implicit private val mode: Mode = Live

  sealed trait SnailFish
  case class Number(value: Int) extends SnailFish
  case class Pair(left: SnailFish, right: SnailFish) extends SnailFish

  def parse(input: String): SnailFish = {
    input match {
      case n if n.length == 1 => Number(n.toInt)
      case pair =>
        val (left, right) = splitPair(pair)
        Pair(parse(left), parse(right))
    }
  }

  def splitPair(string: String): (String, String) = {
    val removeBraces = string.tail.init
    if (removeBraces.head.isDigit) removeBraces.head.toString -> removeBraces.drop(2)
    else {
      takeUntilClosingChar(removeBraces.tail)
    }
  }

  def takeUntilClosingChar(string: String, acc: String = "[", counter: Int = 1): (String, String) = {
    if (counter == 0) acc -> string.drop(1)
    else {
      string.head match {
        case '['       => takeUntilClosingChar(string.tail, acc :+ '[', counter + 1)
        case ']'       => takeUntilClosingChar(string.tail, acc :+ ']', counter - 1)
        case otherwise => takeUntilClosingChar(string.tail, acc :+ otherwise, counter)
      }
    }
  }

  def add(left: SnailFish, right: SnailFish): SnailFish = reduce(Pair(left, right))

  def reduce(snailFish: SnailFish): SnailFish = {
    val (newFish, _, _, exploded) = explode(snailFish)
    if (exploded) reduce(newFish)
    else {
      val (newFish, splitted) = split(snailFish)
      if (splitted) reduce(newFish)
      else snailFish
    }
  }

  def magnitude(snailFish: SnailFish): Int =
    snailFish match {
      case Number(value)     => value
      case Pair(left, right) => (3 * magnitude(left)) + (2 * magnitude(right))
    }

  def explode(snailFish: SnailFish, depth: Int = 0): (SnailFish, Int, Int, Boolean) =
    snailFish match {
      case Pair(Number(left), Number(right)) if depth >= 4 =>
        (Number(0), left, right, true)
      case Pair(left, right) =>
        val (lTree, l1, r1, leftExploded) = explode(left, depth + 1)
        val (rTree, l2, r2, rightExploded) = if (leftExploded) (right, 0, 0, false) else explode(right, depth + 1)
        if (leftExploded) {
          val (newRight, rem) = pushDownR(rTree, r1)
          (Pair(lTree, newRight), l1, rem, true)
        } else if (rightExploded) {
          val (newLeft, rem) = pushDownL(lTree, l2)
          (Pair(newLeft, rTree), rem, r2, true)
        } else
          (Pair(left, right), 0, 0, false)
      case otherwise => (otherwise, 0, 0, false)
    }

  def pushDownL(snailFish: SnailFish, add: Int): (SnailFish, Int) = {
    snailFish match {
      case Number(value) => Number(value + add) -> 0
      case Pair(left, right) =>
        val (rFish, rem1) = pushDownL(right, add)
        val (lFish, rem2) = pushDownL(left, rem1)
        Pair(lFish, rFish) -> rem2
    }
  }

  def pushDownR(snailFish: SnailFish, add: Int): (SnailFish, Int) = {
    snailFish match {
      case Number(value) => Number(value + add) -> 0
      case Pair(left, right) =>
        val (lFish, rem1) = pushDownR(left, add)
        val (rFish, rem2) = pushDownR(right, rem1)
        Pair(lFish, rFish) -> rem2
    }
  }

  def split(snailFish: SnailFish): (SnailFish, Boolean) =
    snailFish match {
      case Number(value) if value >= 10 =>
        Pair(Number(value / 2), Number(if (value % 2 == 0) value / 2 else value / 2 + 1)) -> true
      case Pair(left, right) =>
        val (newLeft, leftSplitted) = split(left)
        val (newRight, rightSplitted) = if (leftSplitted) right -> false else split(right)
        Pair(newLeft, newRight) -> (leftSplitted || rightSplitted)
      case otherwise => otherwise -> false
    }

  def pairs(list: List[SnailFish]): Seq[List[SnailFish]] = {
    for {
      x <- list.indices
      y <- list.indices
      if x != y
    } yield List(list(x), list(y))
  }

  def solve1: Int = magnitude(inputLines.map(parse).reduce(add))
  def solve2: Int = pairs(inputLines.map(parse)).map(_.reduce(add)).map(magnitude).max

  def main(args: Array[String]): Unit = {
    val start = System.currentTimeMillis()
    println(s"AOC $year - Day $day")

    val part1 = solve1

    val mid = System.currentTimeMillis()
    println(s"Answer part 1: $part1 [${mid - start}ms]")

    val part2 = solve2
    val end = System.currentTimeMillis()
    println(s"Answer part 2: $part2 [${end - mid}ms]")
  }
}
