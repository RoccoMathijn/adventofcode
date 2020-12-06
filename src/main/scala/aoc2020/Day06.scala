package aoc2020

import scala.io.Source

object Day06 extends App {
  val input: List[List[List[String]]] = Source
    .fromResource("aoc2020/input-day6.txt")
    .getLines()
    .mkString("\n")
    .split("\n\n")
    .map(_.split("\n").map(_.grouped(1).toList).toList)
    .toList

  println(input.map(_.flatten.distinct.size).sum)

  println(input.map(_.reduce(_ intersect _)).map(_.size).sum)
}
