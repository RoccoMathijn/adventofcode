package aoc2022

import util.AocTools
import util.InputGetter.{Example, Live, Mode}

object Day10 extends AocTools(10, 2022) {
//  implicit private val mode: Mode = Example
  implicit private val mode: Mode = Live

  case class State(register: Int, cycle: Int, history: List[Int])

  def execute: State =
    inputLines.foldLeft(State(1, 0, List(1))) { (ctx, instruction) =>
      instruction match {
        case "noop"         => State(ctx.register, ctx.cycle + 1, ctx.register :: ctx.history)
        case s"addx $value" => State(ctx.register + value.toInt, ctx.cycle + 2, ctx.register :: ctx.register :: ctx.history)
      }
    }

  def solve1: Int = {
    val history = execute.history.reverse

    def interestingCycles(acc: List[Int]): List[Int] = {
      val newCycle = acc.head + 40
      if (newCycle > history.size) acc
      else interestingCycles(newCycle :: acc)
    }

    interestingCycles(List(20)).map { cycle =>
      cycle * history(cycle)
    }.sum
  }

  def printHistory(history: List[Int]): Unit = {
    history.drop(1).zipWithIndex.foreach {
      case (x, i) =>
        val pixel = i % 40
        if (x == pixel || x - 1 == pixel || x + 1 == pixel) print("█") else print(" ")
        if (pixel == 39) println("")
    }
  }

  def solve2: Unit = printHistory(execute.history.reverse)

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
