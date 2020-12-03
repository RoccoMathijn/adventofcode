package aoc2020

import scala.collection.immutable
import scala.io.Source

object Day12 extends App {
  val input: immutable.Seq[Int] = Source
    .fromResource("aoc2020/input-day12.txt")
    .getLines()
    .map(_.toInt)
    .toList


}
