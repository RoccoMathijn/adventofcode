package aoc2020

import scala.collection.immutable
import scala.io.Source

object Day15 extends App {
  val input: immutable.Seq[Int] = Source
    .fromResource("aoc2020/input-day15.txt")
    .getLines()
    .map(_.toInt)
    .toList


}
