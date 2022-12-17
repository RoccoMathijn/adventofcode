package aoc2022

import scalax.collection.Graph
import scalax.collection.GraphPredef._
import util.AocTools
import util.InputGetter.{Example, Live, Mode}

import scala.collection.mutable
import scala.language.postfixOps

object Day16 extends AocTools(16, 2022) {
//  implicit private val mode: Mode = Example
  implicit private val mode: Mode = Live

  case class Valve(id: String, flowRate: Int, neighbours: Set[String], open: Boolean = false)

  object Valve {
    def fromString(string: String): Valve = input.find(_.id == string).get
  }

  val input: List[Valve] = inputLines.map {
    case s"Valve $v has flow rate=$r; tunnel leads to valve $vx"  => Valve(v, r.toInt, Set(vx))
    case s"Valve $v has flow rate=$r; tunnels lead to valves $vx" => Valve(v, r.toInt, vx.split(", ").toSet)
  }

  val edges = input.flatMap(v => v.neighbours.map(n => v.id ~ n))
  val graph = Graph(edges: _*)

  val spMap = mutable.Map.empty[(String, String), List[String]]
  def shortestPath(from: String, to: String): List[String] = {
    val List(f, t) = List(from, to).sorted
    spMap.getOrElse(
      f -> t, {
        val answer = (graph.get(f) shortestPathTo graph.get(t)).get.toList.collect { case elem if !elem.toString.contains('~') => elem.toString }.drop(1)
        spMap.addOne(f -> t, answer)
        answer
      }
    )
  }

  // all ways to split a set into two disjoint sets
  def splitSet[A](set: Set[A]): Set[(Set[A], Set[A])] = {
    val subsets = set.subsets.toSet
    (for {
      sa: Set[A] <- subsets
    } yield Set(sa, set.diff(sa))).map(s => s.head -> s.last)
  }

  def traverse(current: String, valves: Set[Valve], minute: Int, pressureReleased: Int, ticks: Int): Set[Int] = {
    valves.flatMap { nv =>
      val path = shortestPath(current, nv.id)
      val distance = path.size
      val newMinute = minute + distance + 1

      if (newMinute > ticks) Set(pressureReleased)
      else {
        val score = nv.flowRate * (ticks - newMinute + 1)
        val newValves = valves.filterNot(_.id == nv.id).filterNot(v => shortestPath(nv.id,v.id).size > (ticks - newMinute - 1))
        traverse(nv.id, newValves, newMinute, score + pressureReleased, ticks)
      }
    } + pressureReleased
  }

  val interestingValves: Set[Valve] = input.filter(v => !v.open && v.flowRate > 0).toSet

  def solve1: Int = traverse(current = "AA", valves = interestingValves, minute = 1, pressureReleased = 0, ticks = 30).max

  def solve2: Int =
    splitSet(interestingValves).map {
      case (forMe, forElephant) =>
        traverse(current = "AA", valves = forMe, minute = 1, pressureReleased = 0, ticks = 26).max +
          traverse(current = "AA", valves = forElephant, minute = 1, pressureReleased = 0, ticks = 26).max
    }.max

  def main(args: Array[String]): Unit = {
    val start = System.currentTimeMillis()
    println(s"AOC 2022 - Day $day")

    val part1 = solve1
    val mid = System.currentTimeMillis()
    println(s"Answer part 1: $part1 [${mid - start}ms]")
    val part2 = solve2
    val end = System.currentTimeMillis()
    println(s"Answer part 2: $part2 [${end - mid}ms]")
  }
}
