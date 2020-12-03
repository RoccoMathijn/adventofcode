package aoc2020

import scala.collection.immutable
import scala.io.Source

object Day22 extends App {
  val input: immutable.Seq[Int] = Source
    .fromResource("aoc2020/input-day22.txt")
    .getLines()
    .map(_.toInt)
    .toList


}
