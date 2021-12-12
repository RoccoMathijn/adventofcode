package aoc2021

import util.AocTools
import util.InputGetter.{Live, Mode}

object Day11 extends AocTools(11, 2021) {
//  implicit private val mode: Mode = Example
  implicit private val mode: Mode = Live

  val input: List[List[Int]] = inputLines.map(_.toList.map(_.toString.toInt))

  def valueOf(point: Point, grid: List[List[Int]]): Int = grid(point.y)(point.x)

  case class Octopus(energy: Int, point: Point)

  val startGrid: Set[Octopus] = (for {
    x <- input.head.indices
    y <- input.indices
    point = Point(x, y)
  } yield Octopus(valueOf(point, input), point)).toSet

  case class Point(x: Int, y: Int)

  def adjacents(point: Point): Set[Point] = {
    (for {
      x <- -1 to 1
      y <- -1 to 1
      if !(x == 0 && y == 0)
    } yield Point(point.x + x, point.y + y)).toSet
  }

  def step(octopusses: Set[Octopus]): (Set[Octopus], Int) = {
    val gains: Set[Octopus] = octopusses.map(octopus => octopus.copy(energy = octopus.energy + 1))

    def flash(octopusses: Set[Octopus], alreadyFlashed: Set[Octopus] = Set.empty): (Set[Octopus], Int) = {
      val (flashingOctopusses, notFlashing) = octopusses.partition(_.energy > 9)

      if (flashingOctopusses.isEmpty) (octopusses ++ alreadyFlashed).map(octopus => if (octopus.energy > 9) octopus.copy(energy = 0) else octopus) -> alreadyFlashed.size
      else {
        val increased = flashingOctopusses.foldLeft(notFlashing) { (octopusses, flashingOctopus) =>
          val adjacentPoints = adjacents(flashingOctopus.point)
          octopusses.map(octopus => if (adjacentPoints.contains(octopus.point)) octopus.copy(energy = octopus.energy + 1) else octopus)
        }
        flash(increased, alreadyFlashed = alreadyFlashed ++ flashingOctopusses)
      }
    }

    flash(gains)
  }

  def printGrid(octopusses: Set[Octopus]): String = {
    input.indices
      .map { y =>
        input.head.indices.map { x =>
          octopusses.find(octopus => octopus.point == Point(x, y)).get.energy
        }.mkString
      }
      .mkString("\n")
  }

  def solve1: Int =
    (1 to 100)
      .foldLeft((startGrid, 1)) { (grid, _) =>
        val (newGrid, flashes) = step(grid._1)
        newGrid -> (flashes + grid._2)
      }
      ._2

  def flashTillSynced(octopusses: Set[Octopus], counter: Int = 1): Int = {
    val (newGrid, n) = step(octopusses)
    if (n == octopusses.size) counter
    else flashTillSynced(newGrid, counter + 1)
  }

  def main(args: Array[String]): Unit = {
    val start = System.currentTimeMillis()
    println(s"AOC $year - Day $day")

    val part1 = solve1

    val mid = System.currentTimeMillis()
    println(s"Answer part 1: $part1 [${mid - start}ms]")

    val part2 = flashTillSynced(startGrid)
    val end = System.currentTimeMillis()
    println(s"Answer part 2: $part2 [${end - mid}ms]")
  }
}
