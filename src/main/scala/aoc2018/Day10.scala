package aoc2018

import scala.io.Source

object Day10 extends App {
  val startTime = System.currentTimeMillis()

  val input: List[((Int, Int), (Int, Int))] = Source
    .fromResource("aoc2018/input-day10.txt")
    .getLines
    .map(parseLine)
    .toList

  def parseLine(rawLine: String): ((Int, Int), (Int, Int)) = {
    val position = rawLine.dropWhile(_ != '<').drop(1)
    val posx = position.takeWhile(_ != ',').trim.toInt
    val posy = position.dropWhile(_ != ',').takeWhile(_ != '>').drop(1).trim.toInt

    val velocity = position.dropWhile(_ != '<').drop(1)
    val vx = velocity.takeWhile(_ != ',').trim.toInt
    val vy = velocity.dropWhile(_ != ',').takeWhile(_ != '>').drop(1).trim.toInt

    (posx -> posy, vx -> vy)
  }

  def ySize(input: List[((Int, Int), (Int, Int))]): Int = {
    val sky = input.map(_._1._2)
    sky.max - sky.min
  }

  def printSky(configuration: List[(Int, Int)]): Unit = {
    val gridXAxis = configuration.map(_._1).min -> configuration.map(_._1).max
    val gridYAxis = configuration.map(_._2).min -> configuration.map(_._2).max

    for {
      y: Int <- gridYAxis._1 to gridYAxis._2
      x: Int <- gridXAxis._1 to gridXAxis._2
    } yield {
      if (configuration.contains((x, y))) print('#')
      else print('.')

      if (x == gridXAxis._2) println()
    }
    println()
  }

  def tick(input: List[((Int, Int), (Int, Int))]): List[((Int, Int), (Int, Int))] = {
    input.map { case ((posx, posy), (vx, vy)) => ((posx+vx, posy+vy), (vx, vy)) }
  }

  val result: (Int, List[((Int, Int), (Int, Int))]) = (0 to 15000).scanLeft(0 -> input){ (newInput, i) =>
    println(i)
    i -> tick(newInput._2)}.minBy(result => ySize(result._2))
  println(result._1)
  printSky(result._2.map(_._1))

  val endTime = System.currentTimeMillis()
  println(s"Runtime: ${(endTime - startTime)/1000} seconds")
}
