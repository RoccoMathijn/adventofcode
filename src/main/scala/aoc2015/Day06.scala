package aoc2015

import util.InputGetter.Live
import util.{AocTools, InputGetter}

object Day06 extends AocTools(6, 2015) {
  implicit val mode: InputGetter.Mode = Live
  val input: Seq[String] = inputLines

  sealed trait State
  case object Off extends State
  case object On extends State

  sealed trait Action
  case object TurnOff extends Action
  case object TurnOn extends Action
  case object Toggle extends Action

  type Grid[X] = List[(List[(X, Int)], Int)]
  type GridOne = Grid[State]
  val startGridOne: GridOne = List.fill(1000)(List.fill(1000)(Off).zipWithIndex).zipWithIndex

  case class Brightness(value: Int)
  type GridTwo = Grid[Brightness]
  val startGridTwo: GridTwo = List.fill(1000)(List.fill(1000)(Brightness(0)).zipWithIndex).zipWithIndex

  object Action {
    def apply(string: String): Action =
      string match {
        case "turn on"  => TurnOn
        case "turn off" => TurnOff
        case "toggle"   => Toggle
      }
  }

  case class Coordinate(x: Int, y: Int)
  case class Instruction(action: Action, from: Coordinate, to: Coordinate)

  def parse(line: String): Instruction = {
    val InstructionPattern = """(turn on|turn off|toggle) (\d+),(\d+) through (\d+),(\d+)""".r
    line match {
      case InstructionPattern(action, x1, y1, x2, y2) => Instruction(Action(action), Coordinate(x1.toInt, y1.toInt), Coordinate(x2.toInt, y2.toInt))
    }
  }

  def traverseGrid[X](grid: Grid[X], instruction: Instruction, actionStrategy: (Action, X) => X): Grid[X] = {
    grid.map {
      case (row, x) =>
        row.map {
          case (state, y) =>
            if (x >= instruction.from.x && x <= instruction.to.x && y >= instruction.from.y && y <= instruction.to.y)
              actionStrategy(instruction.action, state) -> y
            else
              (state, y)
        } -> x
    }
  }

  val actionStrategyPart1: (Action, State) => State = (action: Action, state: State) => {
    action match {
      case TurnOff => Off
      case TurnOn  => On
      case Toggle  => if (state == On) Off else On
    }
  }

  val actionStrategyPart2: (Action, Brightness) => Brightness = (action: Action, brightness: Brightness) => {
    action match {
      case TurnOff => Brightness(math.max(brightness.value - 1, 0))
      case TurnOn  => Brightness(brightness.value + 1)
      case Toggle  => Brightness(brightness.value + 2)
    }
  }

  def main(args: Array[String]): Unit = {
    val start = System.currentTimeMillis()
    println(s"AOC 2015 - Day $day")

    val part1 = input.map(parse).foldLeft(startGridOne)((grid, instruction) => traverseGrid(grid, instruction, actionStrategyPart1)).map(_._1.count(_._1 == On)).sum
    val mid = System.currentTimeMillis()
    println(s"Answer part 1: $part1 [${mid - start}ms]")

    val part2 = input.map(parse).foldLeft(startGridTwo)((grid, instruction) => traverseGrid(grid, instruction, actionStrategyPart2)).map(_._1.map(_._1.value).sum).sum
    val end = System.currentTimeMillis()
    println(s"Answer part 2: $part2 [${end - mid}ms]")
  }
}
