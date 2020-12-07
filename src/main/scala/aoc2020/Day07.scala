package aoc2020

import scala.io.Source

object Day07 extends App {
  val input: Seq[String] = Source
    .fromResource("aoc2020/input-day7.txt")
    .getLines()
    .toList

  case class Line(color: String, content: List[(Int, String)])
  case class Bag(color: String, content: List[Bag])
  def parseLine(input: String): (String, List[(Int, String)]) = {

    val contains = input.split(" ").drop(4).mkString(" ").split(",")
    val contents = contains.flatMap { content =>
      if (content.trim == "no other bags.") None
      else {
        val amount = content.trim.takeWhile(_ != ' ').toInt
        val color = content.trim.split(" ").drop(1).take(2).mkString(" ")
        Some(amount -> color)
      }
    }.toList
    val color = input.split(" ").take(2).mkString(" ")
    color -> contents
  }

  val colorMap: Map[String, List[(Int, String)]] = input.map(parseLine).toMap
  def findContents(color: String, acc: List[Bag]): List[Bag] = {
    val res = colorMap.get(color) match {
      case Some(value) =>
        value.flatMap {
          case (i, str) =>
            val bag = Bag(color = color, content = findContents(str, List.empty))
            acc ++ List.fill(i)(bag)

        }
      case None => List(Bag(color, acc))
    }
    res
  }

  val graph: Seq[Bag] = colorMap.keys.toList.map(key => Bag(key, findContents(key, List.empty)))

  def hasColor(bag: Bag): Boolean = {
    if (bag.content.isEmpty) false
    else if (bag.content.exists(_.color == "shiny gold")) true
    else bag.content.exists(hasColor)
  }

  def countContent(bag: Bag, acc: 0): Int = {
    if (bag.content.isEmpty) acc
    else {
      acc + bag.content.size + bag.content.map(countContent(_, 0)).sum
    }
  }

  println(graph.count(hasColor) - 1)
  println(countContent(graph.find(bag => bag.color == "shiny gold").get, 0))

}
