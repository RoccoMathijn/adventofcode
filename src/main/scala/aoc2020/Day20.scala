package aoc2020

import util.AocTools
import util.InputGetter.{Example, Live, Mode}

object Day20 extends AocTools(20, 2020) {
//  implicit private val mode: Mode = Example
  implicit private val mode: Mode = Live
  val input: List[Tile] = inputBlob
    .split("\n\n")
    .map { tile =>
      val tileLines = tile.split("\n")
      val id = tileLines.head.slice(5, 9).toInt
      val pixels = tileLines.tail.toList.map(_.toList)
      Tile(id, pixels)
    }
    .toList

  val gridDimensions: Int = Math.sqrt(input.length).toInt

  case class Tile(id: Int, pixels: List[List[Char]]) {
    override def toString: String = {
      s"Tile $id:\n" + pixels.map(_.mkString).mkString("\n")
    }

    private lazy val rotateCCW: Tile = Tile(id, pixels.transpose.map(_.reverse))
    private lazy val rotate180: Tile = rotateCCW.rotateCCW
    private lazy val rotate90: Tile = rotate180.rotateCCW
    private lazy val rotations = List(this, rotateCCW, rotate180, rotate90)
    private lazy val flips = rotations.map(tile => tile.copy(pixels = tile.pixels.reverse)) ++ rotations.map(tile => tile.copy(pixels = tile.pixels.map(_.reverse)))
    lazy val allOrientations: Seq[Tile] = rotations ++ flips

    val top: List[Char] = pixels.head
    val bottom: List[Char] = pixels.last
    val left: List[Char] = pixels.map(_.head)
    val right: List[Char] = pixels.map(_.last)
  }

  def isValid(grid: List[List[Tile]]): Boolean = {
//    println("checking validation for grid")
    lazy val verticalBordersAlign: Boolean = grid.forall { row =>
      row.sliding(2).forall { case List(tile1, tile2) => tile1.allOrientations.exists(orientation => tile2.allOrientations.exists(_.left == orientation.right)) }
    }
    lazy val horizontalBordersAlign = grid.transpose.forall {
      _.sliding(2).forall { case List(tile1, tile2) => tile1.allOrientations.exists(orientation => tile2.allOrientations.exists(_.left == orientation.right)) }
    }
    verticalBordersAlign && horizontalBordersAlign
  }

  private val allArrangements: Iterator[List[List[Tile]]] = input.permutations.map(_.grouped(gridDimensions).toList)
//  lazy val validArrangement = allArrangements.find(isValid)

  val corners: Seq[Tile] = input.filter { tile =>
    val allOrientations = input.filterNot(_.id == tile.id).flatMap(_.allOrientations)
    val topAligns: Boolean = allOrientations.exists(_.bottom == tile.top)
    val bottomAligns: Boolean = allOrientations.exists(_.top == tile.bottom)
    val leftAligns: Boolean = allOrientations.exists(_.right == tile.left)
    val rightAligns: Boolean = allOrientations.exists(_.left == tile.right)
    List(topAligns, bottomAligns, leftAligns, rightAligns).count(_ == true) == 2
  }

  def printIds(grid: List[List[Tile]]): Unit = {
    println(grid.map(_.map(_.id)).map(_.mkString("\t")).mkString("\n"))
  }

  def productOfCornerIds(grid: List[List[Tile]]): Long = {
    val topLeft = grid.head.head.id.toLong
    val topRight = grid.head.last.id.toLong
    val bottomLeft = grid.last.head.id.toLong
    val bottomRight = grid.last.last.id.toLong
    topLeft * topRight * bottomLeft * bottomRight
  }

  def main(args: Array[String]): Unit = {
    println(s"AOC 2020 - Day $day")
    val start = System.currentTimeMillis()
    val part1 = corners.map(_.id.toLong).product
    val mid = System.currentTimeMillis()

    println(s"Answer part 1: $part1 [${mid - start}ms]")

    val part2 = ???
    val end = System.currentTimeMillis()
    println(s"Answer part 2: $part2 [${`end` - mid}ms]")
  }
}
