package aoc2022

import aoc2022.Prelude.Point
import util.AocTools
import util.InputGetter._

import scala.collection.parallel.CollectionConverters.ImmutableIterableIsParallelizable

object Day23 extends AocTools(23, 2022) {
//  implicit private val mode: Mode = Example
  implicit private val mode: Mode = Live

  val input: Set[Point] = inputLines
    .map(_.zipWithIndex)
    .zipWithIndex
    .flatMap {
      case (line, y) =>
        line.collect { case ('#', x) => Point(x, y) }
    }
    .toSet

  val order = List("North", "South", "West", "East")

  case class State(elfs: Set[Point], order: List[String])

  def increment(state: State): State = {
    val (toMoveElfs, doNothingElfs) = state.elfs.par.partition(elf => Prelude.eightAdjacencies(elf).intersect(state.elfs).nonEmpty)

    val proposals = toMoveElfs.map { elf: Point =>
      val firstValidDirection = state.order.find {
        case "North" => northAdjacencies(elf).intersect(state.elfs).isEmpty
        case "South" => southAdjacencies(elf).intersect(state.elfs).isEmpty
        case "West"  => westAdjacencies(elf).intersect(state.elfs).isEmpty
        case "East"  => eastAdjacencies(elf).intersect(state.elfs).isEmpty
      }

      val proposal = firstValidDirection match {
        case Some("North") => elf.copy(y = elf.y - 1)
        case Some("South") => elf.copy(y = elf.y + 1)
        case Some("West")  => elf.copy(x = elf.x - 1)
        case Some("East")  => elf.copy(x = elf.x + 1)
        case None          => elf
      }

      elf -> proposal
    }

    val proposalMap = proposals.groupBy(_._2)

    val moved = proposals
      .map { case (elf, proposal) => if (proposalMap(proposal).size > 1) elf else proposal }
      .seq
      .toSet

    State(moved ++ doNothingElfs, state.order.tail :+ state.order.head)
  }

  def play(state: State, round: Int): State =
    if (round == 0) state
    else play(increment(state), round - 1)

  def printState(set: Set[Point]): Unit = {
    (set.minBy(_.y).y to set.maxBy(_.y).y).foreach { y =>
      (set.minBy(_.x).x to set.maxBy(_.x).x).foreach { x =>
        if (set.contains(Point(x, y))) print("#")
        else print(".")
      }
      println("")
    }
  }

  def countEmptyTiles(set: Set[Point]): Int =
    (set.minBy(_.x).x to set.maxBy(_.x).x).map { x =>
      (set.minBy(_.y).y to set.maxBy(_.y).y).map { y =>
        if (set.contains(Point(x, y))) 0 else 1
      }.sum
    }.sum

  val beginState = State(input, order)

  def northAdjacencies(point: Point): Set[Point] = Set(Point(point.x - 1, point.y - 1), Point(point.x, point.y - 1), Point(point.x + 1, point.y - 1))

  def southAdjacencies(point: Point): Set[Point] = Set(Point(point.x - 1, point.y + 1), Point(point.x, point.y + 1), Point(point.x + 1, point.y + 1))

  def westAdjacencies(point: Point): Set[Point] = Set(Point(point.x - 1, point.y - 1), Point(point.x - 1, point.y), Point(point.x - 1, point.y + 1))

  def eastAdjacencies(point: Point): Set[Point] = Set(Point(point.x + 1, point.y - 1), Point(point.x + 1, point.y), Point(point.x + 1, point.y + 1))

  def solve1: Int = countEmptyTiles(play(beginState, 10).elfs)

  def incrementWhileChanged(state: State, round: Int): Int = {
    val newState = increment(state)
    if (newState.elfs == state.elfs) round else incrementWhileChanged(newState, round + 1)
  }

  def solve2: Int = incrementWhileChanged(beginState, 1)

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
