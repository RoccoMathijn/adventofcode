package aoc2022

import util.AocTools
import util.InputGetter.{Live, Mode}

object Day08 extends AocTools(8, 2022) {
//      implicit private val mode: Mode = Example
  implicit private val mode: Mode = Live

  val input: Vector[Vector[Int]] = inputLines.toVector.map(line => line.toVector.map(_.toString.toInt))
  val rowSize: Int = input.head.size
  val colSize: Int = input.transpose.head.size

  def isVisible(x: Int, y: Int) = {
    val outerEdge = x == 0 || y == 0 || x == rowSize - 1 || y == colSize - 1

    lazy val height = input(x)(y)
    lazy val isVisibleFromTheLeft = input(x).take(y).max < height
    lazy val isVisibleFromTheTop = input.transpose.apply(y).take(x).max < height
    lazy val isVisibleFromTheRight = input(x).drop(y + 1).reverse.max < height
    lazy val isVisibleFromTheBottom = input.transpose.apply(y).drop(x + 1).reverse.max < height

    outerEdge || isVisibleFromTheRight || isVisibleFromTheTop || isVisibleFromTheBottom || isVisibleFromTheLeft
  }

  def countUntilHigher(treeLine: Vector[Int], ownHeight: Int, acc: Int = 0): Int = {
    if (treeLine.isEmpty) acc
    else if (treeLine.head < ownHeight) countUntilHigher(treeLine.tail, ownHeight, acc + 1)
    else {
      acc + 1
    }
  }

  def scenicScore(x: Int, y: Int): Int = {
    val height = input(x)(y)
    val scoreUp: Int = countUntilHigher(input.transpose.apply(y).take(x).reverse, height)
    val scoreLeft: Int = countUntilHigher(input(x).take(y).reverse, height)
    val scoreRight: Int = countUntilHigher(input(x).drop(y + 1), height)
    val scoreDown: Int = countUntilHigher(input.transpose.apply(y).drop(x + 1), height)
    scoreUp * scoreLeft * scoreRight * scoreDown
  }

  def solve1: Int =
    (for (i <- 0 until rowSize; j <- 0 until colSize) yield isVisible(i, j)).count(identity)

  def solve2: Int =
    (for (i <- 0 until rowSize; j <- 0 until colSize) yield scenicScore(i, j)).max

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
