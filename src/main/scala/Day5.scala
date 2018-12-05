import scala.io.Source

object Day5 extends App {
  val startTime = System.currentTimeMillis()
  val polymer = Source
    .fromResource("input-day5.txt")
    .toList

  def react(polymer: List[Char], acc: List[Char]): List[Char] = {
    (polymer, acc) match {
      case (Nil, _) => acc
      case (c1 :: cs, Nil) => react(cs, List(c1))
      case (c1 :: cs, c2 :: as) =>
        if (c1 != c2 && c1.toLower == c2.toLower) react(cs, as)
        else react(cs, c1 :: acc)
    }
  }

  val reactedPolymer = react(polymer, List.empty)

  // Part1
  println(s"Answer Part 1: ${reactedPolymer.length}")

  // Part2
  val shortestPolymerAfterRemovingOneUnit: Int = ('a' to 'z').map(unit =>
    react(reactedPolymer.filterNot(c => c.toLower == unit), List.empty).length
  ).min

  val endTime = System.currentTimeMillis()
  println(s"Answer Part 2: $shortestPolymerAfterRemovingOneUnit")
  println(s"Time in ms: ${endTime - startTime}")
}
