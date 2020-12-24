package aoc2020

import util.AocTools
import util.InputGetter.{Live, Mode}

import scala.annotation.tailrec

object Day24 extends AocTools(24, 2020) {
//  implicit private val mode: Mode = Example
  implicit private val mode: Mode = Live

  val input: Seq[String] = inputLines

  sealed trait Direction
  case object E extends Direction
  case object SE extends Direction
  case object SW extends Direction
  case object W extends Direction
  case object NW extends Direction
  case object NE extends Direction

  case object Direction {
    val values = List(E, SE, SW, W, NW, NE)
  }

  def traverse(position: (Int, Int), direction: Direction): (Int, Int) = {
    (position, direction) match {
      case ((x, y), E)  => x + 1 -> y
      case ((x, y), SE) => (x + 1) -> (y - 1)
      case ((x, y), SW) => x -> (y - 1)
      case ((x, y), W)  => x - 1 -> y
      case ((x, y), NW) => x - 1 -> (y + 1)
      case ((x, y), NE) => x -> (y + 1)
    }
  }

  @tailrec
  def parse(input: List[Char], output: List[Direction]): List[Direction] = {
    input match {
      case Nil              => output.reverse
      case 'e' :: xs        => parse(xs, E :: output)
      case 's' :: 'e' :: xs => parse(xs, SE :: output)
      case 's' :: 'w' :: xs => parse(xs, SW :: output)
      case 'w' :: xs        => parse(xs, W :: output)
      case 'n' :: 'w' :: xs => parse(xs, NW :: output)
      case 'n' :: 'e' :: xs => parse(xs, NE :: output)
    }
  }

  def identifyTile(directions: List[Direction]): (Int, Int) = {
    directions.foldLeft((0, 0))((acc, direction) => traverse(acc, direction))
  }

  def tilesToConsider(blackTiles: Set[(Int, Int)]): Seq[(Int, Int)] = {
    val minX = blackTiles.minBy(_._1)._1 - 1
    val maxX = blackTiles.maxBy(_._1)._1 + 1
    val minY = blackTiles.minBy(_._2)._2 - 1
    val maxY = blackTiles.maxBy(_._2)._2 + 1
    for {
      x <- minX to maxX
      y <- minY to maxY
    } yield (x, y)
  }

  def evolve(blackTiles: Set[(Int, Int)]): Set[(Int, Int)] = {
    val grid = tilesToConsider(blackTiles)
    grid.flatMap(applyRules(_, blackTiles)).toSet
  }

  def applyRules(tile: (Int, Int), blackTiles: Set[(Int, Int)]): Option[(Int, Int)] = {
    val neighbours = Direction.values.map(traverse(tile, _))
    val blackNeighbours = neighbours.count(blackTiles.contains)
    if (blackTiles(tile)) {
      if (blackNeighbours == 0 || blackNeighbours > 2) None else Some(tile)
    } else {
      if (blackNeighbours == 2) Some(tile) else None
    }
  }

  def evolveDays(blackTiles: Set[(Int, Int)])(days: Int): Set[(Int, Int)] =
    (1 to days).foldLeft(blackTiles)((newTiles, _) => evolve(newTiles))

  def main(args: Array[String]): Unit = {
    val start = System.currentTimeMillis()
    println(s"AOC 2020 - Day $day")

    val part1 = input.map(line => parse(line.toList, List.empty)).map(identifyTile).groupBy(identity).values.count(_.size % 2 != 0)
    val mid = System.currentTimeMillis()
    println(s"Answer part 1: $part1 [${mid - start}ms]")

    val initialBlackTiles = input.map(line => parse(line.toList, List.empty)).map(identifyTile).groupBy(identity).filter(_._2.size % 2 != 0).keys.toSet
    val part2 = evolveDays(initialBlackTiles)(100).size
    val end = System.currentTimeMillis()
    println(s"Answer part 2: $part2 [${`end` - mid}ms]")
  }
}
