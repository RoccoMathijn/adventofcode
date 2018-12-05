import scala.io.Source

object Day5 extends App {
  val polymer = Source
    .fromResource("input-day5.txt")
    .getLines()
    .toList
    .head

  def react(polymer: String): String = {
      polymer.foldLeft(" ")((acc, c) =>
        if ((acc.last.isUpper == c.isLower && acc.last.toLower == c) || (acc.last.isLower == c.isUpper && acc.last.toUpper == c)) acc.init
        else acc :+ c).trim
  }

  // Part1
  val lengthAfterReaction: Int = react(polymer).length
  println(lengthAfterReaction)

  def reactOneUnit(polymer: String, unit: Char): String = {
    polymer.foldLeft(" ")((acc, c) =>
      if (c.toLower == unit && (acc.last.isUpper == c.isLower && acc.last.toLower == c) || (acc.last.isLower == c.isUpper && acc.last.toUpper == c)) acc.init
      else acc :+ c).trim
  }

  // Part2
  val shortestPolymerAfterReactingOneUnit = ('a' to 'z').map { unit =>
    polymer.filterNot(c => c.toLower == unit)
  }.map(react).map(_.length).min

  println(shortestPolymerAfterReactingOneUnit)
}
