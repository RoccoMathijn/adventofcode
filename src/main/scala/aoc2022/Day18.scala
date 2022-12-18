package aoc2022

import util.AocTools
import util.InputGetter.{Live, Mode}

object Day18 extends AocTools(18, 2022) {
//      implicit private val mode: Mode = Example
  implicit private val mode: Mode = Live

  case class Cube(x: Int, y: Int, z: Int)
  val lavaDroplet = inputLines.map { line =>
    val Array(x, y, z) = line.split(',')
    Cube(x.toInt, y.toInt, z.toInt)
  }.toSet
  
  private def surfaceArea(cubes: Set[Cube]): Int = {
    (6 * cubes.size) - cubes.toList.map { cube =>
      cubes.count { otherCube => (math.abs(otherCube.x - cube.x) + math.abs(otherCube.y - cube.y) + math.abs(otherCube.z - cube.z)) == 1 }
    }.sum
  }

  def neighbours(cube: Cube): Set[Cube] = {
    Set(cube.copy(x = cube.x - 1), cube.copy(x = cube.x + 1), cube.copy(y = cube.y - 1), cube.copy(y = cube.y + 1), cube.copy(z = cube.z - 1), cube.copy(z = cube.z + 1))
  }

  def outOfBounds(cube: Cube): Boolean = cube.x < xMin || cube.x > xMax || cube.y < yMin || cube.y > yMax || cube.z < zMin || cube.z > zMax
  
  def expand(air: Set[Cube]): Set[Cube] = {
    val newAir = air.flatMap(neighbours).diff(lavaDroplet).filterNot(outOfBounds)
    if (newAir == air) air
    else expand(newAir)
  }
  
  private val xMin: Int = lavaDroplet.map(_.x).min - 1
  private val xMax: Int = lavaDroplet.map(_.x).max + 1
  val xAxis = xMin to xMax
  private val yMin: Int = lavaDroplet.map(_.y).min - 1
  private val yMax: Int = lavaDroplet.map(_.y).max + 1
  val yAxis = yMin to yMax
  private val zMin: Int = lavaDroplet.map(_.z).min - 1
  private val zMax: Int = lavaDroplet.map(_.z).max + 1
  val zAxis = zMin to zMax

  val outerLayerAir: Set[Cube] = {
    val bottom = (for {
      x <- xAxis
      z <- zAxis
    } yield Cube(x, 0, z)).toSet

    val left = (for {
      y <- yAxis
      z <- zAxis
    } yield Cube(0, y, z)).toSet

    val front = (for {
      x <- xAxis
      y <- yAxis
    } yield Cube(x, y, 0)).toSet

    bottom ++ left ++ front ++ bottom.map(_.copy(y = yAxis.max)) ++ bottom.map(_.copy(x = xAxis.max)) ++ front.map(_.copy(z = zAxis.max))
  }

  val all = (for {
    x <- xAxis
      y<-yAxis
    z<-zAxis
  } yield Cube(x, y, z)).toSet

  def solve1: Int = surfaceArea(lavaDroplet)

  def solve2: Int = {
    val expanded = expand(outerLayerAir) 
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
