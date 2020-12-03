package aoc2020

import scala.io.Source

object Day04 extends App {
  val input: List[Int] = Source
    .fromResource("aoc2020/input-day4.txt")
    .getLines()
    .map(_.toInt)
    .toList

}
