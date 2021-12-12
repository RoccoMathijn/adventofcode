package aoc2021

import util.AocTools
import util.InputGetter.{Live, Mode}

object Day12 extends AocTools(12, 2021) {
//  implicit private val mode: Mode = Example
  implicit private val mode: Mode = Live

  val input: Set[(String, String)] = inputLines.map {
    case s"$from-$to" => from -> to
  }.toSet
  val connectionMap: Map[String, Set[String]] = (input ++ input.map { case (str, str1) => str1 -> str }).groupBy(_._1).view.mapValues(_.map(_._2)).toMap

  def mapPaths(from: String, Map: Map[String, Set[String]]): Set[List[String]] = {
    if (from == "end") Set(List("end"))
    else {
      val singleVisitCave = from.forall(_.isLower)
      val reachableCaves: Set[String] = Map.getOrElse(from, Set.empty)
      val filteredMap = if (singleVisitCave) Map.filterNot(_._1 == from) else Map

      for {
        to <- reachableCaves
        xs <- mapPaths(to, filteredMap)
      } yield from +: to +: xs
    }
  }

  def mapPaths2(from: String, Map: Map[String, Set[String]], jokerUsed: Boolean): Set[List[String]] = {
    if (from == "end") Set(List("end"))
    else {
      val singleVisitCave = from.forall(_.isLower)
      val reachableCaves: Set[String] = Map.getOrElse(from, Set.empty)
      val startFilteredMap = Map.filterNot(_._1 == "start")

      val filteredMap = startFilteredMap.filterNot(_._1 == from)

      if (singleVisitCave && jokerUsed)
        for {
          to <- reachableCaves
          xs <- mapPaths(to, filteredMap)
        } yield from +: to +: xs
      else if (singleVisitCave) {
        val useJoker = for {
          to <- reachableCaves
          xs <- mapPaths(to, startFilteredMap)
        } yield from +: to +: xs

        val dontUseJoker =
          for {
            to <- reachableCaves
            xs <- mapPaths2(to, filteredMap, jokerUsed = false)
          } yield from +: to +: xs

        useJoker ++ dontUseJoker
      } else
        for {
          to <- reachableCaves
          xs <- mapPaths2(to, startFilteredMap, jokerUsed)
        } yield from +: to +: xs
    }
  }

  def solve1: Int = mapPaths("start", Map = connectionMap).size
  def solve2: Int = mapPaths2("start", connectionMap, jokerUsed = false).size

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
