package aoc2019

import scala.io.Source

object Day03 extends App {
  val input: List[List[String]] = Source
    .fromResource("aoc2019/input-day3.txt")
    .getLines()
    .map(_.split(',').toList)
    .toList

  def parseWireOp(input: String): (String, Int) = {
    val (direction, distance) = input.splitAt(1)
    direction -> distance.toInt
  }

  def toLine(rawLine: List[String],
             curPos: (Int, Int),
             acc: List[(Int, Int)]): List[(Int, Int)] = {
    rawLine match {
      case Nil => acc
      case x :: xs =>
        parseWireOp(x) match {
          case ("R", distance) =>
            toLine(
              xs,
              (curPos._1, curPos._2 + distance),
              acc ++ (1 to distance).map(y => (curPos._1, curPos._2 + y))
            )
          case ("U", distance) =>
            toLine(
              xs,
              (curPos._1 + distance, curPos._2),
              acc ++ (1 to distance).map(x => (curPos._1 + x, curPos._2))
            )
          case ("L", distance) =>
            toLine(
              xs,
              (curPos._1, curPos._2 - distance),
              acc ++ (1 to distance).map(y => (curPos._1, curPos._2 - y))
            )
          case ("D", distance) =>
            toLine(
              xs,
              (curPos._1 - distance, curPos._2),
              acc ++ (1 to distance).map(x => (curPos._1 - x, curPos._2))
            )
        }
    }
  }

  def toLineWithStep(rawLine: List[String],
                     curPos: (Int, Int),
                     step: Int,
                     acc: List[(Int, Int, Int)]): List[(Int, Int, Int)] = {
    rawLine match {
      case Nil => acc
      case x :: xs =>
        parseWireOp(x) match {
          case ("R", distance) =>
            toLineWithStep(
              xs,
              (curPos._1, curPos._2 + distance),
              step + Math.abs(distance),
              acc ++ (1 to distance).map(
                y => (curPos._1, curPos._2 + y, step + y)
              )
            )
          case ("U", distance) =>
            toLineWithStep(
              xs,
              (curPos._1 + distance, curPos._2),
              step + Math.abs(distance),
              acc ++ (1 to distance).map(
                x => (curPos._1 + x, curPos._2, step + x)
              )
            )
          case ("L", distance) =>
            toLineWithStep(
              xs,
              (curPos._1, curPos._2 - distance),
              step + Math.abs(distance),
              acc ++ (1 to distance).map(
                y => (curPos._1, curPos._2 - y, step + y)
              )
            )
          case ("D", distance) =>
            toLineWithStep(
              xs,
              (curPos._1 - distance, curPos._2),
              step + Math.abs(distance),
              acc ++ (1 to distance).map(
                x => (curPos._1 - x, curPos._2, step + x)
              )
            )
        }
    }
  }

  def manhattanDistance(pointA: (Int, Int), pointB: (Int, Int)): Int =
    Math.abs(pointA._1 - pointB._1) + Math.abs(pointA._2 - pointB._2)

//  val line1 = toLine(input.head, (0,0), List.empty)
//  val line2 = toLine(input.last, (0,0), List.empty)

  val line1 = "R75,D30,R83,U83,L12,D49,R71,U7,L72".split(',').toList
  val line2 = "U62,R66,U55,R34,D71,R55,D58,R83".split(',').toList

//  println(line1.intersect(line2).minBy(x => manhattanDistance(x, (0,0))))

  val line1WithStep = toLineWithStep(input.head, (0, 0), 0, List.empty)
  val line2WithStep = toLineWithStep(input.last, (0, 0), 0, List.empty)

  val res = line1WithStep
    .flatMap {
      case (x, y, s) =>
        line2WithStep
          .filter { case (x2, y2, _) => x == x2 && y == y2 }
          .map(l2i => (x, y, s) -> l2i)
    }
    .minBy { case (p1, p2) => p1._3 + p2._3 }

  println(res._1._3 + res._2._3)
}
