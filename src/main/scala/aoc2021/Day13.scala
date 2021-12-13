package aoc2021

import util.AocTools
import util.InputGetter.{Example, Live, Mode}

object Day13 extends AocTools(13, 2021) {
  implicit private val mode: Mode = Example
//  implicit private val mode: Mode = Live

  case class Coordinate(x: Int, y: Int)
  val dots: Set[Coordinate] = inputLines
    .takeWhile(_.nonEmpty)
    .map { line =>
      val split = line.split(',')
      Coordinate(split(0).toInt, split(1).toInt)
    }
    .toSet

  def fold(dots: Set[Coordinate], instructions: List[Fold]): Set[Coordinate] = {
    instructions.foldLeft(dots)((dots, instruction) =>
      instruction match {
        case X(n) => dots.map(c => if (c.x > n) c.copy(x = n - (c.x - n)) else c)
        case Y(n) => dots.map(c => if (c.y > n) c.copy(y = n - (c.y - n)) else c)
      }
    )
  }

  sealed trait Fold
  case class X(value: Int) extends Fold
  case class Y(value: Int) extends Fold

  val foldInstructions: List[Fold] = inputLines
    .dropWhile(_.nonEmpty)
    .drop(1)
    .map {
      case s"fold along x=$n" => X(n.toInt)
      case s"fold along y=$n" => Y(n.toInt)
    }

  def toString(dots: Set[Coordinate]): String =
    "\n" +
      (0 to dots.maxBy(_.y).y)
        .map(y => (0 to dots.maxBy(_.x).x).map(x => dots.find(dot => dot.x == x && dot.y == y).map(_ => "#").getOrElse(".")).mkString)
        .mkString("\n")

  def solve1: Int = fold(dots, foldInstructions.take(1)).size
  def solve2: String = toString(fold(dots, foldInstructions))

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
