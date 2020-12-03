package aoc2020

import scala.collection.immutable
import scala.io.Source

object Day09 extends App {
  val input: immutable.Seq[Int] = Source
    .fromResource("aoc2020/input-day9.txt")
    .getLines()
    .map(_.toInt)
    .toList


}
