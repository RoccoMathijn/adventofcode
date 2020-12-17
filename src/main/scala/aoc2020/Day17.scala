package aoc2020

import util.AocTools
import util.InputGetter.{Example, Live, Mode}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.matching.Regex

object Day17 extends AocTools(17, 2020) {
//  implicit private val mode: Mode = Example
  implicit private val mode: Mode = Live

  val input: Seq[String] = inputLines
  type Position = (Int, Int, Int, Int)

  type PocketDimension = Set[(Int, Int, Int, Int)]
  val initialState: PocketDimension = input.zipWithIndex.flatMap {
    case (line, y) =>
      line.toCharArray.zipWithIndex.flatMap {
        case (c, x) =>
          if (c == '#') Some((x, y, 0, 0))
          else None
      }
  }.toSet

  def neighBours(position: Position): Seq[(Int, Int, Int, Int)] = {
    (for {
      x <- -1 to 1
      y <- -1 to 1
      z <- -1 to 1
      w <- -1 to 1
      if !(x == 0 && y == 0 && z == 0 && w == 0)
    } yield (position._1 + x, position._2 + y, position._3 + z, position._4 + w))
  }

  def countActiveNeighbours(position: Position, pocketDimension: PocketDimension): Int = {
    neighBours(position).count(pocketDimension.contains)
  }

  def allConsiderablePositions(pocketDimension: PocketDimension): Seq[(Int, Int, Int, Int)] = {
    val minX = pocketDimension.minBy(_._1)._1 - 1
    val minY = pocketDimension.minBy(_._2)._2 - 1
    val minZ = pocketDimension.minBy(_._3)._3 - 1
    val minW = pocketDimension.minBy(_._4)._4 - 1
    val maxX = pocketDimension.maxBy(_._1)._1 + 1
    val maxY = pocketDimension.maxBy(_._2)._2 + 1
    val maxZ = pocketDimension.maxBy(_._3)._3 + 1
    val maxW = pocketDimension.maxBy(_._4)._4 + 1

    for {
      x <- minX to maxX
      y <- minY to maxY
      z <- minZ to maxZ
      w <- minW to maxW
    } yield (x, y, z, w)
  }

  def runOnce(pocketDimension: PocketDimension): PocketDimension = {
    allConsiderablePositions(pocketDimension).flatMap { position =>
      val activeNeighbours = countActiveNeighbours(position, pocketDimension)
      if (pocketDimension(position)) {
        if (activeNeighbours == 2 || activeNeighbours == 3)
          Some(position)
        else
          None
      } else {
        if (activeNeighbours == 3)
          Some(position)
        else {
          None
        }
      }
    }.toSet
  }

  @tailrec
  def runSixCycles(pocketDimension: PocketDimension, cycle: Int): PocketDimension = {
    if (cycle == 6) pocketDimension
    else {
      println(s"After $cycle cycle:")
      runSixCycles(runOnce(pocketDimension), cycle + 1)
    }
  }

  def main(args: Array[String]): Unit = {
    println(s"AOC 2020 - Day $day")
    val start = System.currentTimeMillis()

    val part1 = runSixCycles(initialState, 0).size
    val mid = System.currentTimeMillis()

    println(s"Answer part 1: $part1 [${mid - start}ms]")

    val part2 = ???
    val end = System.currentTimeMillis()
    println(s"Answer part 2: $part2 [${`end` - mid}ms]")
  }
}
