package aoc2021

import util.AocTools
import util.InputGetter.{Example, Live, Mode}

import scala.util.Try

object Day20 extends AocTools(20, 2021) {
//  implicit private val mode: Mode = Example
  implicit private val mode: Mode = Live

  val imageEnhancementAlgorithm: Vector[Char] = inputLines.head.toVector
  val inputImage: Vector[Vector[Char]] = inputLines.toVector.drop(2).map(_.toVector).filter(_.nonEmpty)

  case class Point(x: Int, y: Int)

  def nine(point: Point): Seq[Point] =
    for {
      y <- -1 to 1
      x <- -1 to 1
    } yield Point(point.x + x, point.y + y)

  def valueOf(point: Point, image: Vector[Vector[Char]], iteration: Int): Char =
    Try(image(point.y)(point.x)).getOrElse {
      if (iteration % 2 == 0) imageEnhancementAlgorithm.head
      else if (imageEnhancementAlgorithm.head == '.') '.'
      else imageEnhancementAlgorithm.last
    }

  def step(image: Vector[Vector[Char]], n: Int): Vector[Vector[Char]] = {
    (-1 to image.size).toVector.map(y =>
      (-1 to image.head.size).toVector.map { x =>
        newChar(image, y, x, n)
      }
    )
  }

  def newChar(image: Vector[Vector[Char]], y: Int, x: Int, n: Int) = {
    val binaryNumber = nine(Point(x, y))
      .map(valueOf(_, image, n))
      .map {
        case '.' => '0'
        case '#' => '1'
      }
      .mkString
    imageEnhancementAlgorithm(Integer.parseInt(binaryNumber, 2))
  }

  def apply(times: Int): Vector[Vector[Char]] = {
    (1 to times).foldLeft(inputImage)((image, n) => step(image, n))
  }

  def count(image: Vector[Vector[Char]]): Int =
    image.flatten.count(_ == '#')
  def solve1 = count(apply(2))
  def solve2 = count(apply(50))

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
