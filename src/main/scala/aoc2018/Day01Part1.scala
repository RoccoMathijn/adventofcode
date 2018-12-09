package aoc2018

import scala.io.Source

object Day01Part1 extends App {
  val result = Source.fromResource("aoc2018/input-day1.txt").getLines().map(Integer.parseInt).sum
  println(result)
}
