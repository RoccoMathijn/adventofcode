package aoc2020

import util.AocTools
import util.InputGetter.{Example, Live, Mode}

import scala.annotation.tailrec

object Day12 extends AocTools(12, 2020) {
//  implicit private val mode: Mode = Example
  implicit private val mode: Mode = Live

  def parseLine(line: String): Instruction = {
    val instruction = line.head
    val value = line.tail.toInt
    instruction match {
      case 'N' => N(value)
      case 'S' => S(value)
      case 'E' => E(value)
      case 'W' => W(value)
      case 'L' => L(value)
      case 'R' => R(value)
      case 'F' => F(value)
    }
  }
  val input: List[Instruction] = inputLines.map(parseLine)

  sealed trait Instruction {
    val value: Int
  }
  case class N(value: Int) extends Instruction
  case class S(value: Int) extends Instruction
  case class E(value: Int) extends Instruction
  case class W(value: Int) extends Instruction
  case class L(value: Int) extends Instruction
  case class R(value: Int) extends Instruction
  case class F(value: Int) extends Instruction

  case class State(location: (Int, Int), direction: Char, wayPoint: (Int, Int))

  @tailrec
  def run(input: List[Instruction], state: State): State = {
    if (input.isEmpty) state
    else {
      input.head match {
        case N(value) => run(input.tail, state.copy(location = state.location._1 -> (state.location._2 + value)))
        case S(value) => run(input.tail, state.copy(location = state.location._1 -> (state.location._2 - value)))
        case E(value) => run(input.tail, state.copy(location = (state.location._1 + value) -> state.location._2))
        case W(value) => run(input.tail, state.copy(location = (state.location._1 - value) -> state.location._2))
        case L(value) => run(input.tail, state.copy(direction = rotateL(state.direction, value)))
        case R(value) => run(input.tail, state.copy(direction = rotateR(state.direction, value)))
        case F(value) => run(parseLine(s"${state.direction}${value.toString}") +: input.tail, state)
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

  @tailrec
  def runWithWayPoint(input: List[Instruction], state: State): State = {
    input match {
      case Nil => state
      case x :: xs =>
        x match {
          case N(value) => runWithWayPoint(xs, state.copy(wayPoint = state.wayPoint._1 -> (state.wayPoint._2 + value)))
          case S(value) => runWithWayPoint(xs, state.copy(wayPoint = state.wayPoint._1 -> (state.wayPoint._2 - value)))
          case E(value) => runWithWayPoint(xs, state.copy(wayPoint = (state.wayPoint._1 + value) -> state.wayPoint._2))
          case W(value) => runWithWayPoint(xs, state.copy(wayPoint = (state.wayPoint._1 - value) -> state.wayPoint._2))
          case L(value) => runWithWayPoint(xs, state.copy(wayPoint = rotateWayPointL(state.wayPoint, value)))
          case R(value) => runWithWayPoint(xs, state.copy(wayPoint = rotateWayPointR(state.wayPoint, value)))
          case F(value) =>
            val newLocation = (state.wayPoint._1 * value + state.location._1, state.wayPoint._2 * value + state.location._2)
            runWithWayPoint(xs, state.copy(location = newLocation))
        }
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
