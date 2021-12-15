package aoc2021

import util.AocTools
import util.InputGetter.{Example, Live, Mode}

object Day14 extends AocTools(14, 2021) {
  implicit private val mode: Mode = Example
//  implicit private val mode: Mode = Live

  val template: String = inputLines.head

  val insertionRules: Map[String, Char] = inputLines
    .drop(2)
    .map {
      case s"$elements -> $element" => elements -> element.head
    }
    .toMap

  val initialPairCount: Map[String, Long] = template.toSeq.sliding(2).map(_.mkString).toList.groupMapReduce(identity)(_ => 1L)(_ + _)
  val initialCharCount: Map[Char, Long] = template.toSeq.groupMapReduce(identity)(_ => 1L)(_ + _)

  val insertionResults: Map[String, List[String]] = insertionRules.map {
    case (pair, char) =>
      pair -> List(s"${pair.head}$char", s"$char${pair.last}")
  }

  private def pairInsertion(pairCounts: Map[String, Long], charCounts: Map[Char, Long]): (Map[String, Long], Map[Char, Long]) = {
    val newPairs: Map[String, Long] = pairCounts.toList
      .flatMap(kv => insertionResults(kv._1).map(_ -> kv._2))
      .groupMapReduce(_._1)(_._2)(_ + _)

    val newCharCounts: List[(Char, Long)] = pairCounts.toList.map(kv => insertionRules(kv._1) -> kv._2)
    val totalCharCounts = (charCounts.toSeq ++ newCharCounts).groupMapReduce(_._1)(_._2)(_ + _)
    (newPairs, totalCharCounts)
  }

  def solve(n: Int): Long = {
    val polymer = (0 until n).foldLeft(initialPairCount -> initialCharCount)((acc, _) => pairInsertion(acc._1, acc._2))._2
    polymer.values.max - polymer.values.min
  }

  def solve1: Long = solve(10)
  def solve2: Long = solve(40)

  def main(args: Array[String]): Unit = {
    val start = System.currentTimeMillis()
    println(s"AOC $year - Day $day")

    val part1 = solve1

    val mid = System.currentTimeMillis()
    println(s"Answer part 1: $part1 [${mid - start}ms]")

    val part2 = solve2
    val end = System.currentTimeMillis()
    println(s"Answer part 2: $part2 [${end - mid}ms]")
  }
}
