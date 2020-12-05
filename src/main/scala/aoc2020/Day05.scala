package aoc2020

import scala.io.Source

object Day05 extends App {
  val input: Seq[Int] = Source
    .fromResource("aoc2020/input-day5.txt")
    .getLines()
    .map(toBin _ andThen toInt)
    .toList

  def toInt(input: String): Int = {
    Integer.parseInt(input, 2)
  }

  def toBin(input: String): String = {
    input.map {
      case 'F' | 'L' => 0
      case 'B' | 'R' => 1
    }.mkString
  }

  println(input.max)

  input.sorted.fold(79) { (acc, i) =>
    if (i != acc + 1) println(acc)
    i
  }
}
