package aoc2018

import scala.io.Source

object Day6 extends App {
  val startTime = System.currentTimeMillis()
  val coordinates = Source
    .fromResource("aoc2018/input-day6.txt")
    .getLines
    .toList
    .zipWithIndex
    .map { case (rawString, index) => parseCoordinates(rawString, index) }
  val TARGET_VALUE = 10000

  case class Coordinate(identifier: Int, pos: Pos)
  case class Pos(x: Int, y: Int)

  def parseCoordinates(rawString: String, index: Int): Coordinate = {
    val Array(x, y) = rawString.split(',').map(_.trim).map(Integer.parseInt)
    Coordinate(index, Pos(x, y))
  }

  def manhattanDistance(pos1: Pos, pos2: Pos): Int = {
    Math.abs(pos1.x - pos2.x) + Math.abs(pos1.y - pos2.y)
  }

  def closestCoordinates(x: Int, y: Int): Seq[Int] = {
    coordinates
      .map(c => c.identifier -> manhattanDistance(c.pos, Pos(x, y)))
      .groupBy { case (_, distance) => distance }
      .minBy { case (distance, _) => distance }
      ._2
      .map(_._1)
  }

  val gridSize: Pos = Pos(coordinates.maxBy(_.pos.x).pos.x, coordinates.maxBy(_.pos.y).pos.y)

  val closestCoordinatesPerPos: Seq[(Pos, Int)] = for {
    x <- 0 to gridSize.x
    y <- 0 to gridSize.y
    coordinatesClosest = closestCoordinates(x, y)
    if coordinatesClosest.length == 1
  } yield {
    Pos(x, y) -> coordinatesClosest.head
  }

  val coordinatesWithInfiniteAreas = closestCoordinatesPerPos
    .filter { case (Pos(x, y), _) => x == 0 || y == 0 || x == gridSize.x || y == gridSize.y }
    .map(_._2)
    .distinct

  val biggestArea = closestCoordinatesPerPos
    .filterNot(x => coordinatesWithInfiniteAreas.contains(x._2)).map(_._2)
    .groupBy(identity)
    .values
    .map(_.length)
    .max

  println(s"Answer part 1: $biggestArea")


  val positionsCloseToCoordinates = for {
    y <- 0 to gridSize.y
    x <- 0 to gridSize.x
    sumMHDistance = coordinates.map(c => manhattanDistance(c.pos, Pos(x, y))).sum
    if sumMHDistance < TARGET_VALUE
  } yield {
    Pos(x, y)
  }

  println(s"Answer part 2: ${positionsCloseToCoordinates.length}")

  val endTime = System.currentTimeMillis()
  println(s"Runtime: ${endTime - startTime}")
}
