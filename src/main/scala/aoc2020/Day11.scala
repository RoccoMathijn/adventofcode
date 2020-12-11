package aoc2020

import util.AocTools
import util.InputGetter.{Example, Live, Mode}

object Day11 extends AocTools(11, 2020) {
//  implicit private val mode: Mode = Example
  implicit private val mode: Mode = Live

  type Layout = List[List[Position]]

  def printLayout(layout: Layout): Unit = {
    layout.foreach { row =>
      row.foreach {
        case _: Floor    => print('.')
        case _: Empty    => print('L')
        case _: Occupied => print('#')
      }
      println("")
    }
  }

  val input: Layout = inputLines.zipWithIndex.map(row =>
    row._1.zipWithIndex.map {
      case ('.', x) => Floor(x, row._2)
      case ('L', x) => Empty(x, row._2)
      case ('#', x) => Occupied(x, row._2)
    }.toList
  )

  sealed trait Position {
    val x: Int
    val y: Int
  }
  case class Floor(x: Int, y: Int) extends Position
  case class Empty(x: Int, y: Int) extends Position
  case class Occupied(x: Int, y: Int) extends Position

  def runUntilUnChanged(layout: Layout, part2: Boolean): Layout = {
    val next = nextLayout(layout, part2)
    if (next == layout) next
    else runUntilUnChanged(next, part2)
  }

  def countOccupied(layout: Layout): Int = layout.map(_.count(_.isInstanceOf[Occupied])).sum

  def nextLayout(layout: Layout, part2: Boolean): Layout = {
    val visibilityMethod = if (part2) visibleSeats _ else adjacentSeats _
    layout.map(_.map(position => {
      val adjacentSeats = visibilityMethod(position.x -> position.y, layout)
      applyRules(position, adjacentSeats, part2)
    }))
  }

  def applyRules(position: Position, adjacentSeats: List[Position], part2: Boolean): Position = {
    val threshold = if (part2) 5 else 4
    position match {
      case Empty(x, y) if adjacentSeats.isEmpty              => Occupied(x, y)
      case Occupied(x, y) if adjacentSeats.size >= threshold => Empty(x, y)
      case otherwise                                         => otherwise
    }
  }

  def adjacentSeats(position: (Int, Int), layout: Layout): List[Position] = {
    val columns: Seq[Int] = List(position._1 - 1, position._1, position._1 + 1).filterNot(_ < 0).filterNot(_ >= input.head.size)
    val rows: Seq[Int] = List(position._2 - 1, position._2, position._2 + 1).filterNot(_ < 0).filterNot(_ >= input.size)

    (for {
      x <- columns
      y <- rows
      if (x, y) != position
    } yield layout(y)(x)).filter(_.isInstanceOf[Occupied]).toList
  }

  //  Here be dragons
  def visibleSeats(position: (Int, Int), layout: Layout): List[Position] = {
    val column = layout.map(_(position._1))
    val up: Option[Position] = column.take(position._2).reverse.find(!_.isInstanceOf[Floor])
    val down: Option[Position] = column.drop(position._2 + 1).find(!_.isInstanceOf[Floor])
    val row = layout(position._2)
    val left: Option[Position] = row.take(position._1).reverse.find(!_.isInstanceOf[Floor])
    val right: Option[Position] = row.drop(position._1 + 1).find(!_.isInstanceOf[Floor])

    val lu = upL(position, List.empty).map(p => layout(p._2)(p._1)).find(!_.isInstanceOf[Floor])
    val ru = upR(position, List.empty).map(p => layout(p._2)(p._1)).find(!_.isInstanceOf[Floor])
    val ld = downL(position, List.empty).map(p => layout(p._2)(p._1)).find(!_.isInstanceOf[Floor])
    val rd = downR(position, List.empty).map(p => layout(p._2)(p._1)).find(!_.isInstanceOf[Floor])

    List(up, down, left, right, lu, ru, ld, rd).flatten.filter(_.isInstanceOf[Occupied])
  }

  def upL(position: (Int, Int), acc: List[(Int, Int)]): List[(Int, Int)] = {
    val newX = position._1 - 1
    val newY = position._2 - 1
    if (newX < 0 || newY < 0) acc
    else upL(newX -> newY, acc :+ newX -> newY)
  }

  def upR(position: (Int, Int), acc: List[(Int, Int)]): List[(Int, Int)] = {
    val newX = position._1 + 1
    val newY = position._2 - 1
    if (newX >= input.head.size || newY < 0) acc
    else upR(newX -> newY, acc :+ newX -> newY)
  }

  def downL(position: (Int, Int), acc: List[(Int, Int)]): List[(Int, Int)] = {
    val newX = position._1 - 1
    val newY = position._2 + 1
    if (newX < 0 || newY >= input.size) acc
    else downL(newX -> newY, acc :+ newX -> newY)
  }

  def downR(position: (Int, Int), acc: List[(Int, Int)]): List[(Int, Int)] = {
    val newX = position._1 + 1
    val newY = position._2 + 1
    if (newX >= input.head.size || newY >= input.size) acc
    else downR(newX -> newY, acc :+ newX -> newY)
  }

  def main(args: Array[String]): Unit = {
    println(s"AOC 2020 - Day $day")

    val part1 = countOccupied(runUntilUnChanged(input, part2 = false))
    println(s"Answer part 1: $part1")

    val part2 = countOccupied(runUntilUnChanged(input, part2 = true))
    println(s"Answer part 2: $part2")
  }
}
