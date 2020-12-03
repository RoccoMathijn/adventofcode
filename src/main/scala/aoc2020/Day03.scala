package aoc2020

import scala.collection.immutable
import scala.io.Source
import scala.util.control.Breaks._
object Day03 extends App {
  val input: immutable.Seq[String] = Source
    .fromResource("aoc2020/input-day3.txt")
    .getLines()
    .toList

  val stepX = 1
  val stepY = 2

  val mapX = input.head.length
  val mapY = input.length

  def isTree(x: Int, y: Int): Boolean = {
    input(y)(x) == '#'
  }

  def nextStep(x: Int, y: Int): (Int, Int) = {
    val newX = (x + stepX) % mapX
    val newY = y + stepY

    (newX, newY)
  }

  var count = 0
  var nStep = (0, 0)

  while (true) {
    if (nStep._2 >= mapY) {
      print(count)
      break
    }
    if (isTree(nStep._1, nStep._2)) {
      count += 1
    }
    nStep = nextStep(nStep._1, nStep._2)
  }
}
