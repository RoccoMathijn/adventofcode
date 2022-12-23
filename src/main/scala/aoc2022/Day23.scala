package aoc2022

import aoc2022.Prelude.Point
import util.AocTools
import util.InputGetter._

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
  def play(state: State, round: Int): State = {
    if (round == 0) state
    else {
      val (toMoveElfs, doNothingElfs) = state.elfs.partition(elf => Prelude.eightAdjacencies(elf).intersect(state.elfs).nonEmpty)
      
      val proposals: Set[(Point, Point)] = toMoveElfs.map { elf =>
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

      val moved = proposals.map {
        case (elf, proposal) =>
          if (proposalMap(proposal).size > 1) elf else proposal
      }

      play(State(moved ++ doNothingElfs, state.order.tail :+ state.order.head), round - 1)
    }
  }

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

  def northAdjacencies(point: Point): Set[Point] = {
    val topLeft = Point(point.x - 1, point.y - 1)
    val top = Point(point.x, point.y - 1)
    val topRight = Point(point.x + 1, point.y - 1)
    Set(topLeft, top, topRight)
  }

  def southAdjacencies(point: Point): Set[Point] = {
    val topLeft = Point(point.x - 1, point.y + 1)
    val top = Point(point.x, point.y + 1)
    val topRight = Point(point.x + 1, point.y + 1)
    Set(topLeft, top, topRight)
  }

  def westAdjacencies(point: Point): Set[Point] = {
    val topLeft = Point(point.x - 1, point.y - 1)
    val top = Point(point.x - 1, point.y)
    val topRight = Point(point.x - 1, point.y + 1)
    Set(topLeft, top, topRight)
  }

  def eastAdjacencies(point: Point): Set[Point] = {
    val topLeft = Point(point.x + 1, point.y - 1)
    val top = Point(point.x + 1, point.y)
    val topRight = Point(point.x + 1, point.y + 1)
    Set(topLeft, top, topRight)
  }

  def solve1: Int = countEmptyTiles(play(beginState, 10).elfs)

  def incrementWhileChanged(state: State, round: Int): Int = {
    println(s"Round=$round")
    val newState = play(state, 1)
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
