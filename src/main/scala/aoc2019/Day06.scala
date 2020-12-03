package aoc2019

import scala.io.Source

object Day06 extends App {
  val input = Source
    .fromResource("aoc2019/input-day6.txt")
    .getLines()
    .map(line => line.takeWhile(_ != ')') -> line.dropWhile(_ != ')').drop(1))
    .toList

  def countOrbits(obj: String,
                  orbitMap: List[(String, String)],
                  acc: Int): Int = {
    val directOrbits = orbitMap.find(_._2 == obj).map(_._1)
    if (directOrbits.isEmpty) acc
    else directOrbits.map(obj => countOrbits(obj, orbitMap, acc + 1)).sum
  }

  def objects(orbitMap: List[(String, String)]): List[String] = {
    orbitMap.flatMap(o => List(o._1, o._2)).distinct
  }

  def plotGraph(obj: String,
                orbitMap: List[(String, String)],
                graph: List[String]): List[String] = {
    val directOrbits = orbitMap.find(_._2 == obj).map(_._1)
    if (directOrbits.isEmpty) graph
    else plotGraph(directOrbits.get, orbitMap, obj :: graph)
  }

  println(objects(input).map(obj => countOrbits(obj, input, 0)).sum)

  val you = plotGraph("YOU", input, List.empty)
  val san = plotGraph("SAN", input, List.empty)
  println((you.diff(san) ++ san.diff(you)).length - 2)
}
