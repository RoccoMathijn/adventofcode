package aoc2020

import scala.io.Source

object Day07 extends App {
  val input: Seq[String] = Source
    .fromResource("aoc2020/input-day7.txt")
    .getLines()
    .toList

  case class Line(color: String, content: List[(Int, String)])

  def parseLine(input: String): (String, List[(Int, String)]) = {
    val contains = input.split(" ").drop(4).mkString(" ").split(",")
    val contents = contains.flatMap { content =>
      if (content.trim == "no other bags.") None
      else {
        val amount = content.trim.takeWhile(_ != ' ').toInt
        val color = content.trim.split(" ").slice(1, 3).mkString(" ")
        Some(amount -> color)
      }
    }.toList
    val color = input.split(" ").take(2).mkString(" ")
    color -> contents
  }

  val colorMap: Map[String, List[(Int, String)]] = input.map(parseLine).toMap

  def containsShinyGold(color: String): Boolean = {
    colorMap.get(color) match {
      case Some(contents) if contents.exists(_._2 == "shiny gold") => true
      case Some(contents)                                          => contents.exists(x => containsShinyGold(x._2))
      case None                                                    => false
    }
  }

  def countContent(color: String, acc: Int): Int = {
    val contents = colorMap(color)
    if (contents.isEmpty) acc
    else contents.map { case (i, color) => acc + i * (1 + countContent(color, 0)) }.sum
  }

  println(colorMap.keys.count(containsShinyGold))
  println(countContent("shiny gold", 0))
}
