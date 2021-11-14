package aoc2016

import util.AocTools
import util.InputGetter.Live

object Day01 extends AocTools(1, 2016) {
//  implicit val mode = Example
  implicit val mode = Live
  type Instructions = Seq[(Char, Int)]
  val instructions: Instructions = inputLines.head.split(',').map(_.trim).map(instruction => instruction.head -> instruction.tail.toInt).toList

  sealed trait Direction
  case object North extends Direction
  case object East extends Direction
  case object South extends Direction
  case object West extends Direction

  case class State(x: Int, y: Int, direction: Direction)

  def applyInstruction(state: State, instruction: (Char, Int)): State = {
    instruction match {
      case ('L', n) =>
        state match {
          case State(x, y, North) => State(x - n, y, West)
          case State(x, y, East)  => State(x, y + n, North)
          case State(x, y, South) => State(x + n, y, East)
          case State(x, y, West)  => State(x, y - n, South)
        }
      case ('R', n) =>
        state match {
          case State(x, y, North) => State(x + n, y, East)
          case State(x, y, East)  => State(x, y - n, South)
          case State(x, y, South) => State(x - n, y, West)
          case State(x, y, West)  => State(x, y + n, North)
        }
    }
  }

  def intermediateStations(state1: State, state2: State): List[(Int, Int)] = {
    math.min(state1.x, state2.x) to math.max(state1.x, state2.x) flatMap { x =>
      math.min(state1.y, state2.y) to math.max(state1.y, state2.y) map { y =>
        (x, y)
      }
    }
  }.toList.diff(List(state1.x -> state1.y))

  def run(state: State, instructions: Instructions, history: List[(Int, Int)]): (Int, Int) = {
    val newState = applyInstruction(state, instructions.head)
    val intermediate = intermediateStations(state, newState)
    if (intermediate.intersect(history).nonEmpty) {
      intermediate.intersect(history).head
    } else {
      run(newState, instructions.tail, history ++ intermediate)
    }
  }

  val beginState: State = State(x = 0, y = 0, direction = North)

  def main(args: Array[String]): Unit = {
    val start = System.currentTimeMillis()
    println(s"AOC 2016 - Day $day")

    val hq = instructions.foldLeft(State(0, 0, North))((state, instruction) => applyInstruction(state, instruction))
    val part1 = hq.x + hq.y
    val mid = System.currentTimeMillis()
    println(s"Answer part 1: $part1 [${mid - start}ms]")

    val hq2 = run(State(0, 0, North), instructions, List.empty)
    val part2 = math.abs(hq2._1) + math.abs(hq2._2)
    val end = System.currentTimeMillis()

    println(s"Answer part 2: $part2 [${end - mid}ms]")
  }
}
