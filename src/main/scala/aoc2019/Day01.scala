package aoc2019

import scala.collection.immutable
import scala.io.Source

object Day01 extends App {
  val input: immutable.Seq[Int] = Source
    .fromResource("aoc2019/input-day1.txt")
    .getLines()
    .map(_.toInt)
    .toList

  def rocketEquationDoubleChecker(mass: Int): Int = mass / 3 - 2

  def rocketEquationWithFuel(mass: Int, acc: Int): Int = {
    val neededFuel = rocketEquationDoubleChecker(mass)
    if (neededFuel <= 0) acc
    else rocketEquationWithFuel(neededFuel, acc + neededFuel)
  }

  val result1: Int = input
    .map(rocketEquationDoubleChecker)
    .sum

  val result2 = input
    .map(mass => rocketEquationWithFuel(mass, 0))
    .sum

  println(result1)
  println(result2)
}
