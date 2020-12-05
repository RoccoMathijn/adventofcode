package aoc2020

import scala.collection.immutable
import scala.io.Source

object Day05 extends App {
  val input: immutable.Seq[String] = Source
    .fromResource("aoc2020/input-day5.txt")
    .getLines()
    .map(toBin)
    .toList

  def toInt(input: String): Int = {
    Integer.parseInt(input, 2)
  }

  def toBin(input:String): String = {
    input.map {
      case 'F' => 0
      case 'B' => 1
      case 'R' => 1
      case 'L' => 0
    }.mkString
  }

  def toId(input: String): Int = {
    val row = toInt(input.take(7))
    val column = toInt(input.drop(7))

    row*8+column
  }

  println(input.map(toId).max)

  input.map(toId).sorted.foldLeft(79){(acc,i) =>
    if (i != acc + 1) println(acc)
    i
  }

}
