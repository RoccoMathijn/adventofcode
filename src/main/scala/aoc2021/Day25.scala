package aoc2021

import util.AocTools
import util.InputGetter.{Example, Live, Mode}

import scala.collection.mutable

object Day25 extends AocTools(25, 2021) {
//  implicit private val mode: Mode = Example
  implicit private val mode: Mode = Live

  type Grid = mutable.ListBuffer[mutable.ListBuffer[Char]]
  val startGrid: List[List[Char]] = inputLines.map(_.toList)

  val grid: Grid = mutable.ListBuffer.from(startGrid).map(l => mutable.ListBuffer.from(l))
  print
  
  def stepEast(x: Int, y: Int, startGrid: List[List[Char]]): Unit = {
    if (y == grid.size) ()
    else if (x >= grid.head.size) stepEast(0, y + 1, startGrid)
    else {
      val char = grid(y)(x)
      val nextEast = (x + 1) % grid.head.size
//      println(s"$y, $x: $char; Next east: $nextEast")
      if (char == '>') {
        if (startGrid(y)(nextEast) == '.') {
          grid(y)(nextEast) = '>'
          grid(y)(x) = '.'
          stepEast(x + 2, y, startGrid)
        } else {
          stepEast(x + 1, y, startGrid)
        }
      } else stepEast(x + 1, y, startGrid)
    }
  }

  def stepSouth(x: Int, y: Int, startGrid: List[List[Char]]): Unit = {
    if (x == grid.head.size) ()
    else if (y >= grid.size) stepSouth(x + 1, 0, startGrid)
    else {
      val char = grid(y)(x)
      val nextSouth = (y + 1) % grid.size
//      println(s"$y, $x: $char; Next south: $nextSouth")
      if (char == 'v') {
        if (startGrid(nextSouth)(x) == '.') {
          grid(nextSouth)(x) = 'v'
          grid(y)(x) = '.'
          stepSouth(x, y + 2, startGrid)
        } else {
          stepSouth(x, y + 1, startGrid)
        }
      } else stepSouth(x, y + 1, startGrid)
    }
  }

  def step: Unit = {
    stepEast(0, 0, grid.map(_.toList).toList)
    stepSouth(0, 0, grid.map(_.toList).toList)
  }
  
  def print = {
    grid.toList.map(_.toList) foreach( line => println(line.mkString))
    println()
  }
  
  def loopUntilNoneChange(n: Int): Int = {
    val startGrid = grid.toList.map(_.toList)

    step
    if(grid.toList.map(_.toList) == startGrid) {
      print
      n
    }
    else loopUntilNoneChange(n+1)
  }

  
  def solve1 = loopUntilNoneChange(1)
  def solve2 = ""

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
