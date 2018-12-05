import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
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

  val reactedPolymer = react(polymer)

  // Part1
  val lengthAfterReaction: Int = reactedPolymer.length
  println(lengthAfterReaction)

  // Part2
  val shortestPolymerAfterRemovingOneUnit: Future[Int] = Future.sequence(('a' to 'z').map { unit =>
    Future {
      react(reactedPolymer.filterNot(c => c.toLower == unit)).length
    }
  }).map(_.min)

  shortestPolymerAfterRemovingOneUnit onComplete println

  Thread.sleep(20000)
}
