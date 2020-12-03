package aoc2019

import scala.io.Source

object Day08 extends App {
  type Layer = List[Char]
  val input: List[Layer] = Source
    .fromResource("aoc2019/input-day8.txt")
    .getLines()
    .mkString
    .grouped(25*6)
    .toList
    .map(_.toList)

  val layerWithFewestZeros: Layer = input.minBy(_.count(c => c == '0'))
  val ones = layerWithFewestZeros.count(_ =='1')
  val twos = layerWithFewestZeros.count(_ == '2')

  println(ones * twos)

  input.transpose.map(_.dropWhile(_ == '2').head).grouped(25).foreach(line => println(line.mkString))
}
