import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.io.Source

object Day5 extends App {
  val polymer = Source
    .fromResource("input-day5.txt")
    .getLines
    .mkString

  def react(polymer: String): String = {
    polymer.foldRight(" ")((c, acc) => {
      val head = acc.head
      if (head != c && head.toLower == c.toLower) acc.tail
      else c +: acc
    }).trim
  }

  val reactedPolymer = react(polymer)

  // Part1
  println(reactedPolymer.length)

  // Part2
  val shortestPolymerAfterRemovingOneUnit: Future[Int] = Future.sequence(('a' to 'z').map (unit =>
    Future {
      react(reactedPolymer.filterNot(c => c.toLower == unit)).length
    }
  )).map(_.min)

  shortestPolymerAfterRemovingOneUnit onComplete println

  Thread.sleep(20000)
}
