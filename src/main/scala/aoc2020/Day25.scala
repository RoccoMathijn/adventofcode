package aoc2020

import scala.collection.immutable
import scala.io.Source

object Day25 extends App {
  val input: immutable.Seq[Int] = Source
    .fromResource("aoc2020/input-day25.txt")
    .getLines()
    .map(_.toInt)
    .toList


}
