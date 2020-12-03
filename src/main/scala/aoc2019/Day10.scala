package aoc2019

import scala.io.Source

object Day10 extends App {
  val input: List[(Int, Int)] = Source
    .fromResource("aoc2019/input-day10.txt")
    .getLines()
    .map(_.toList.zipWithIndex)
    .toList
    .zipWithIndex
    .map(y => y._1.map(x => (x._1 -> (x._2, y._2))))
    .map(_.filter(_._1 == '#'))
    .map(_.map(_._2))
    .filter(_.nonEmpty)
    .flatten


  def lineOfSight(pointA: (Int, Int), pointB: (Int, Int)): List[(Int, Int)] = {
    val distanceA = Math.abs(pointA._1 - pointB._1)
    val distanceB = Math.abs(pointA._2 - pointB._2)

    val gcdivisor = gcd(
      distanceA,
      distanceB
    )

    println(s"GCD: $gcdivisor")
    println(s"PointA: $pointA")
    println(s"PointB: $pointB")
    println(s"Distance A: $distanceA")
    println(s"Distance B: $distanceB")
//    println(s"Distance A / GCD: ${distanceA/gcdivisor}")
//    println(s"Distance B / GCD: ${distanceB/gcdivisor}")

    val xCos = (Math.min(pointA._1, pointB._1) to Math.max(pointA._1, pointB._1)).filter(x => distanceA == 0 || x % (distanceA / gcdivisor) == 0)
    println(s"xCos: $xCos")
    val yCos = (Math.min(pointA._2, pointB._2) to Math.max(pointA._2, pointB._2)).filter(y => distanceB == 0 || y % (distanceB / gcdivisor) == 0)
    println(s"yCos: $yCos")
    xCos.zip(yCos).filter(cos => cos != pointA && cos != pointB).toList
  }

  def gcd(a: Int, b: Int): Int = {
    if (b == 0) a else gcd(b, a % b)
  }

  def countVisible(grid: List[(Int, Int)], pointA: (Int, Int)): Int = {
    grid.count(pointB => !lineOfSight(pointA, pointB).exists(grid.contains))
  }



//  println(lineOfSight((0,0), (9, 9)))
//  println(lineOfSight((1,0), (3, 4)))
//  println(lineOfSight((1,0), (3, 4)))
//  println(input)
//  println(input.map(countVisible(input, _)))

  println(lineOfSight((0,4), (4,4)))
}
