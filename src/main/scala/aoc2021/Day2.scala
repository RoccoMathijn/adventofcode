package aoc2021

import util.AocTools
import util.InputGetter.{Example, Live, Mode}

object Day2 extends AocTools(2, 2021) {
//    implicit private val mode: Mode = Example
  implicit private val mode: Mode = Live
  val Forward = "forward (\\d)".r
  val Down = "down (\\d)".r
  val Up = "up (\\d)".r
  case class State1(horizontal: Int, depth: Int)
  val result1 = inputLines.foldLeft(State1(0, 0)) { (state, command) =>
    command match {
      case Forward(i) => State1(state.horizontal + i.toInt, state.depth)
      case Down(i)    => State1(state.horizontal, state.depth + i.toInt)
      case Up(i)      => State1(state.horizontal, state.depth - i.toInt)
    }
  }
  val part1 = result1.horizontal * result1.depth

  case class State2(horizontal: Int, depth: Int, aim: Int)
  val result2 = inputLines.foldLeft(State2(0, 0, 0)) { (state, command) =>
    command match {
      case Forward(i) => State2(state.horizontal + i.toInt, state.depth + (state.aim * i.toInt), state.aim)
      case Down(i)    => State2(state.horizontal, state.depth, state.aim + i.toInt)
      case Up(i)      => State2(state.horizontal, state.depth, state.aim - i.toInt)
    }
  }
  val part2 = result2.horizontal * result2.depth

  def main(args: Array[String]): Unit = {
    val start = System.currentTimeMillis()
    println(s"AOC 2021 - Day $day")

    val mid = System.currentTimeMillis()
    println(s"Answer part 1: $part1 [${mid - start}ms]")
    val end = System.currentTimeMillis()
    println(s"Answer part 2: $part2 [${end - mid}ms]")
  }
}
