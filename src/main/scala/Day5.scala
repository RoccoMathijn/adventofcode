import scala.io.Source

object Day5 extends App {
  val startTime = System.currentTimeMillis()
  val polymer = Source
    .fromResource("input-day5.txt")
    .toList

  def react(polymer: List[Char]): List[Char] = {
    polymer.foldRight(List.empty[Char]) { (c1, acc) =>
      val c2Option = acc.headOption
      if (c2Option.exists(c2 => c1 != c2 && c1.toLower == c2.toLower)) acc.tail
      else c1 +: acc
    }
  }

  val reactedPolymer = react(polymer)

  // Part1
  println(s"Answer Part 1: ${reactedPolymer.length}")

  // Part2
  val shortestPolymerAfterRemovingOneUnit: Int = ('a' to 'z').map(unit =>
    react(reactedPolymer.filterNot(c => c.toLower == unit)).length
  ).min

  val endTime = System.currentTimeMillis()
  println(s"Answer Part 2: $shortestPolymerAfterRemovingOneUnit")
  println(s"Time in ms: ${endTime - startTime}")
}
