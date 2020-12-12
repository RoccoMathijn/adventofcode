package aoc2020

import util.AocTools
import util.InputGetter.{Example, Live, Mode}

import scala.annotation.tailrec

object Day12 extends AocTools(12, 2020) {
//  implicit private val mode: Mode = Example
  implicit private val mode: Mode = Live

  def parseLine(line: String): Instruction = Instruction(line.head, line.tail.toInt)
  val input: List[Instruction] = inputLines.map(parseLine)

  case class Instruction(command: Char, value: Int)
  case class State(location: (Int, Int), direction: Char, wayPoint: (Int, Int))

  @tailrec
  def run(input: List[Instruction], state: State): State = {
    input match {
      case Nil => state
      case x :: xs =>
        x match {
          case Instruction('N', value) => run(xs, state.copy(location = state.location._1 -> (state.location._2 + value)))
          case Instruction('S', value) => run(xs, state.copy(location = state.location._1 -> (state.location._2 - value)))
          case Instruction('E', value) => run(xs, state.copy(location = (state.location._1 + value) -> state.location._2))
          case Instruction('W', value) => run(xs, state.copy(location = (state.location._1 - value) -> state.location._2))
          case Instruction('L', value) => run(xs, state.copy(direction = rotateL(state.direction, value)))
          case Instruction('R', value) => run(xs, state.copy(direction = rotateR(state.direction, value)))
          case Instruction('F', value) => run(Instruction(state.direction, value) :: xs, state)
        }
    }
  }

  @tailrec
  def runWithWayPoint(input: List[Instruction], state: State): State = {
    input match {
      case Nil => state
      case x :: xs =>
        x match {
          case Instruction('N', value) => runWithWayPoint(xs, state.copy(wayPoint = state.wayPoint._1 -> (state.wayPoint._2 + value)))
          case Instruction('S', value) => runWithWayPoint(xs, state.copy(wayPoint = state.wayPoint._1 -> (state.wayPoint._2 - value)))
          case Instruction('E', value) => runWithWayPoint(xs, state.copy(wayPoint = (state.wayPoint._1 + value) -> state.wayPoint._2))
          case Instruction('W', value) => runWithWayPoint(xs, state.copy(wayPoint = (state.wayPoint._1 - value) -> state.wayPoint._2))
          case Instruction('L', value) => runWithWayPoint(xs, state.copy(wayPoint = rotateWayPointL(state.wayPoint, value)))
          case Instruction('R', value) => runWithWayPoint(xs, state.copy(wayPoint = rotateWayPointR(state.wayPoint, value)))
          case Instruction('F', value) =>
            val newLocation = (state.wayPoint._1 * value + state.location._1, state.wayPoint._2 * value + state.location._2)
            runWithWayPoint(xs, state.copy(location = newLocation))
        }
    }
  }

  val directions = List('N', 'E', 'S', 'W')

  def rotateL(direction: Char, degrees: Int): Char = {
    directions.reverse((directions.reverse.indexOf(direction) + (degrees / 90)) % 4)
  }

  def rotateR(direction: Char, degrees: Int): Char = {
    directions((directions.indexOf(direction) + (degrees / 90)) % 4)
  }

  @tailrec
  def rotateWayPointL(wayPoint: (Int, Int), degrees: Int): (Int, Int) = {
    if (degrees == 0) wayPoint
    else {
      rotateWayPointL(-wayPoint._2 -> wayPoint._1, degrees - 90)
    }
  }

  @tailrec
  def rotateWayPointR(wayPoint: (Int, Int), degrees: Int): (Int, Int) = {
    if (degrees == 0) wayPoint
    else {
      rotateWayPointR(wayPoint._2 -> -wayPoint._1, degrees - 90)
    }
  }

  def manhattanDistance(location: (Int, Int)): Int = {
    math.abs(location._1) + math.abs((location._2))
  }

  def main(args: Array[String]): Unit = {
    println(s"AOC 2020 - Day $day")

    val part1 = manhattanDistance(run(input, State((0, 0), 'E', (0, 0))).location)
    println(s"Answer part 1: $part1")

    val part2 = manhattanDistance(runWithWayPoint(input, State((0, 0), 'E', (10, 1))).location)
    println(s"Answer part 2: $part2")
  }
}
