package aoc2018

import scala.io.Source

object Day02Part1 extends App {
  val boxIds = Source.fromResource("aoc2018/input-day2.txt").getLines()

  val grouped: List[List[Int]] = boxIds.map(_.groupBy(c => c).values.map(_.length).toList).toList
  grouped.foreach(println)

  val twos = grouped.count(_.contains(2))
  val threes = grouped.count(_.contains(3))
  println(s"2: $twos")
  println(s"3: $threes")
  println(s"$twos * $threes = ${twos * threes}")
}
