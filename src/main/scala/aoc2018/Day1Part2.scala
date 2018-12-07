package aoc2018

import scala.collection.immutable
import scala.io.Source

object Day1Part2 extends App {
  val freqList: immutable.Seq[Int] = Source.fromResource("input-day1.txt").getLines().map(Integer.parseInt).toList
  val infiniteRunningTotal: Seq[Int] = Stream.continually(freqList.toStream).flatten.scanLeft(0)(_+_)
  def loop(left: Seq[Int], right: Seq[Int]): Int = {
    if (left.contains(right.head)) right.head else loop(left :+ right.head, right.tail)
  }
  println(loop(infiniteRunningTotal.take(1), infiniteRunningTotal.tail))
}
