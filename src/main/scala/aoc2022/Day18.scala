package aoc2022

import util.AocTools
import util.InputGetter.{Example, Live, Mode}

object Day18 extends AocTools(18, 2022) {
//  implicit private val mode: Mode = Example
  implicit private val mode: Mode = Live

  case class Voxel(x: Int, y: Int, z: Int)
  val lavaDroplet = inputLines.map { line =>
    val Array(x, y, z) = line.split(',')
    Voxel(x.toInt, y.toInt, z.toInt)
  }.toSet

  private def surfaceArea(shape: Set[Voxel]): Int = {
    (6 * shape.size) - shape.toList.map { cube =>
      shape.count { otherCube => (math.abs(otherCube.x - cube.x) + math.abs(otherCube.y - cube.y) + math.abs(otherCube.z - cube.z)) == 1 }
    }.sum
  }

  def neighbours(cube: Voxel): Set[Voxel] = {
    Set(
      cube.copy(x = cube.x - 1),
      cube.copy(x = cube.x + 1),
      cube.copy(y = cube.y - 1),
      cube.copy(y = cube.y + 1),
      cube.copy(z = cube.z - 1),
      cube.copy(z = cube.z + 1)
    )
  }

  def outOfBounds(cube: Voxel): Boolean = cube.x < xMin || cube.x > xMax || cube.y < yMin || cube.y > yMax || cube.z < zMin || cube.z > zMax

  def expand(air: Set[Voxel]): Set[Voxel] = {
    val newAir = air ++ air.flatMap(neighbours).diff(lavaDroplet).filterNot(outOfBounds)
    if (newAir == air) air
    else expand(newAir)
  }

  private val xMin: Int = lavaDroplet.map(_.x).min - 1
  private val xMax: Int = lavaDroplet.map(_.x).max + 1
  private val yMin: Int = lavaDroplet.map(_.y).min - 1
  private val yMax: Int = lavaDroplet.map(_.y).max + 1
  private val zMin: Int = lavaDroplet.map(_.z).min - 1
  private val zMax: Int = lavaDroplet.map(_.z).max + 1
  private val genesis = Voxel(xMin, yMin, zMin)

  val all = (for {
    x <- xMin to xMax
    y <- yMin to yMax
    z <- zMin to zMax
  } yield Voxel(x, y, z)).toSet

  def solve1: Int = surfaceArea(lavaDroplet)

  def solve2: Int = {
    val expanded = expand(Set(genesis))
    solve1 - surfaceArea(all -- lavaDroplet -- expanded)
  }

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
