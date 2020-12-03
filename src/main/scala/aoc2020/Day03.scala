package aoc2020

import scala.collection.immutable
import scala.io.Source

object Day03 extends App {
  val input: immutable.Seq[String] = Source
    .fromResource("aoc2020/input-day3.txt")
    .getLines()
    .toList

  val stepX = 1
  val stepY = 2

  def isTree(x: Int, y: Int): Boolean = {
    input(y)(x) == '#'
  }

  def nextStep(x: Int, y: Int): (Int, Int) = {
    val newX = (x + stepX) % input.head.length
    val newY = y + stepY

    (newX, newY)
  }

  var finished = false
  var count = 0
  var nStep = (0, 0)

  while (!finished) {
    if (nStep._2 >= input.length) {
      finished = true
    } else if (isTree(nStep._1, nStep._2)) {
      count += 1
    }
    nStep = nextStep(nStep._1, nStep._2)
  }
  print(count)
}
