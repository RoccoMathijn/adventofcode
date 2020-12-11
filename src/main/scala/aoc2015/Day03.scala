package aoc2015

import util.AocTools
import util.InputGetter.{Example, Live}

object Day03 extends AocTools(3, 2015) {
//  implicit val mode = Example
  implicit val mode = Live
  val input: List[Char] = inputLines.map(_.toList).head

  def visit(input: List[Char], currentLocation: (Int, Int), houses: Map[(Int, Int), Int]): Map[(Int, Int), Int] = {
    if (input.isEmpty) houses
    else {
      val newLocation = input.head match {
        case '>' => (currentLocation._1 + 1) -> currentLocation._2
        case '<' => (currentLocation._1 - 1) -> currentLocation._2
        case '^' => currentLocation._1 -> (currentLocation._2 + 1)
        case 'v' => currentLocation._1 -> (currentLocation._2 - 1)
      }
      val gifts: Int = houses.getOrElse(newLocation, 0) + 1
      visit(input.tail, newLocation, houses.updated(newLocation, gifts))
    }
  }

  def main(args: Array[String]): Unit = {
    val startPos = (0, 0)
    val startMap = Map(startPos -> 1)
    println(visit(input, startPos, startMap).size)

    val (santaInput, roboInput) = input.zipWithIndex.partition(_._2 % 2 == 0)
    val santaHouses = visit(santaInput.map(_._1), startPos, startMap)
    val combined = visit(roboInput.map(_._1), startPos, santaHouses)

    println(combined.size)
  }
}
