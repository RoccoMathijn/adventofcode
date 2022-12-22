package aoc2022

import aoc2022.Day22Helper._
import aoc2022.Prelude.Point
import util.AocTools
import util.InputGetter._

object Day22 extends AocTools(22, 2022) {
//  implicit private val mode: Mode = Example
  implicit private val mode: Mode = Live

  val input: List[List[Char]] = inputLines.takeWhile(_.nonEmpty).map(_.toList)
  val maxX = input.map(_.size).max + 1
  val board = input.map(row => pad(row, maxX))

  def pad(list: List[Char], max: Int): List[Char] = if (list.size < max) list ++ List.fill(max - list.size)(' ') else list

  println(s"Fixed size: ${board.size} by ${board.head.size}")

  val instructions = parseInstruction(inputLines.last.toList, List.empty).reverse
  println(instructions)

  def parseInstruction(input: List[Char], acc: List[Instruction]): List[Instruction] = {
    input match {
      case Nil         => acc
      case 'R' :: rest => parseInstruction(rest, TurnRight :: acc)
      case 'L' :: rest => parseInstruction(rest, TurnLeft :: acc)
      case otherwise   => parseInstruction(otherwise.dropWhile(_.isDigit), Forward(otherwise.takeWhile(_.isDigit).mkString.toInt) :: acc)
    }
  }

  def printHistory(history: List[(Point, Facing)], board: List[List[Char]]): Unit = {
    board.indices.foreach { y =>
      board.head.indices.foreach { x =>
        val char = history
          .findLast(e => e._1 == Point(x, y))
          .map { facing =>
            facing._2 match {
              case Day22Helper.Up    => '^'
              case Day22Helper.Down  => 'v'
              case Day22Helper.Left  => '<'
              case Day22Helper.Right => '>'
            }
          }
          .getOrElse(board(y)(x))
        print(char)
      }
      println("")
    }
  }

  val beginState = {
    val startPoint = Point(board.head.zipWithIndex.find(_._1 == '.').get._2, 0)
    val facing = Right
    State(startPoint, facing, List(startPoint -> facing))
  }

  def stepThrough(instructions: List[Instruction], state: State): State = {
    instructions.foldLeft(state) { (newState, i) =>
      i match {
        case TurnLeft =>
          val newFacing = rotateLeft(newState.facing)
          newState.copy(facing = newFacing, history = newState.history.init :+ newState.history.last._1 -> newFacing)
        case TurnRight =>
          val newFacing = rotateRight(newState.facing)
          newState.copy(facing = newFacing, history = newState.history.init :+ newState.history.last._1 -> newFacing)
        case Forward(steps) => travel(newState, Forward(steps), board)
      }
    }
  }

  def stepThrough2(instructions: List[Instruction], state: State): State = {
    instructions.foldLeft(state) { (newState, i) =>
      i match {
        case TurnLeft =>
          val newFacing = rotateLeft(newState.facing)
          newState.copy(facing = newFacing, history = newState.history.init :+ newState.history.last._1 -> newFacing)
        case TurnRight =>
          val newFacing = rotateRight(newState.facing)
          newState.copy(facing = newFacing, history = newState.history.init :+ newState.history.last._1 -> newFacing)
        case Forward(steps) => travel2(newState, Forward(steps), board)
      }
    }
  }

  def result(state: State): Int = {
    val facingScore = state.facing match {
      case Right => 0
      case Down  => 1
      case Left  => 2
      case Up    => 3
    }

    1000 * (state.position.y + 1) + 4 * (state.position.x + 1) + facingScore
  }

  def rotateLeft(facing: Facing): Facing =
    facing match {
      case Up    => Left
      case Left  => Down
      case Down  => Right
      case Right => Up
    }

  def rotateRight(facing: Facing): Facing =
    facing match {
      case Up    => Right
      case Right => Down
      case Down  => Left
      case Left  => Up
    }

  def solve1: Int = result(stepThrough(instructions, beginState))

  def solve2: Int = result(stepThrough2(instructions, beginState))

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

object Day22Helper {
  sealed trait Instruction
  case class Forward(steps: Int) extends Instruction
  case object TurnLeft extends Instruction
  case object TurnRight extends Instruction

  sealed trait Facing
  case object Up extends Facing
  case object Down extends Facing
  case object Left extends Facing
  case object Right extends Facing

  case class State(position: Point, facing: Facing, history: List[(Point, Facing)] = List.empty)

  def valueAt(point: Point, board: List[List[Char]]): Char = board(point.y)(point.x)

  def wrapAround(i: Int, size: Int): Int = {
    val offset = i % size
    if (offset < 0) offset + size else offset
  }

  def relative(position: Point, board: List[List[Char]]): Point = {
    val relX = position.x - offsetX(position.y, board)
    val relY = position.y - offsetY(position.x, board)
    Point(relX, relY)
  }

  def offsetX(y: Int, board: List[List[Char]]): Int = board(y).takeWhile(_ == ' ').size
  def offsetY(x: Int, board: List[List[Char]]): Int = board.map(_.apply(x)).takeWhile(_ == ' ').size

  def rowSize(y: Int, board: List[List[Char]]): Int = board(y).filterNot(_ == ' ').size
  def colSize(x: Int, board: List[List[Char]]): Int = board.map(_.apply(x)).filterNot(_ == ' ').size

  def travel(state: State, forward: Forward, board: List[List[Char]]): State = {
    forward match {
      case Forward(0) => state
      case Forward(x) =>
        val relativePoint = relative(state.position, board)
        val newPos: Point = state.facing match {
          case Up =>
            Point(state.position.x, wrapAround(relativePoint.y - 1, colSize(state.position.x, board)) + offsetY(state.position.x, board))
          case Down =>
            Point(state.position.x, wrapAround(relativePoint.y + 1, colSize(state.position.x, board)) + offsetY(state.position.x, board))
          case Left =>
            Point(wrapAround(relativePoint.x - 1, rowSize(state.position.y, board)) + offsetX(state.position.y, board), state.position.y)
          case Right =>
            Point(wrapAround(relativePoint.x + 1, rowSize(state.position.y, board)) + offsetX(state.position.y, board), state.position.y)
        }

        if (valueAt(newPos, board) == '#') state
        else if (valueAt(newPos, board) == ' ') throw new Error(s"Empty space $state, $forward")
        else travel(state.copy(newPos, history = state.history :+ (newPos -> state.facing)), Forward(x - 1), board)
    }
  }

  def travel2(state: State, forward: Forward, board: List[List[Char]]): State = {
    forward match {
      case Forward(0) => state
      case Forward(x) =>
        val newPos: Point = state.facing match {
          case Up    => state.position.copy(y = state.position.y - 1)
          case Down  => state.position.copy(y = state.position.y + 1)
          case Left  => state.position.copy(x = state.position.x - 1)
          case Right => state.position.copy(x = state.position.x + 1)
        }
        val (wrapped, newFacing) = if (newPos.x < 0 || newPos.y < 0 || newPos.y >= 200|| valueAt(newPos, board) == ' ') wrapAroundCube(newPos, state.facing) else newPos -> state.facing
        
        if (valueAt(wrapped, board) == '#') state
        else if (valueAt(wrapped, board) == ' ') throw new Error(s"Empty space $state, $forward")
        else travel2(state.copy(position = wrapped, facing = newFacing, history = state.history :+ (wrapped -> newFacing)), Forward(x - 1), board)
    }
  }

  def wrapAroundCube(point: Point, facing: Facing): (Point, Facing) = {
    facing match {
      case Up =>
        // face 1
        if (point.x >= 50 && point.x < 100 && point.y == -1) Point(0, point.x + 100) -> Right
        // face 2
        else if (point.x >= 100 && point.y == -1) Point(point.x - 100, 199) -> Up
        // face 5
        else if (point.x < 50 && point.y == 99) Point(50, point.x + 50) -> Right
        else point -> facing
      case Down =>
        //face 2
        if (point.x >= 100 && point.y == 50) Point(99, point.x - 50) -> Left
        // face 4
        else if (point.x >= 50 && point.y == 150) Point(49, point.x + 100) -> Left
        // face 6
        else if (point.y == 200) Point(point.x + 100, 0) -> Down
        else point -> facing
      case Left =>
        //face 1
        if (point.x == 49 && point.y < 50) Point(0, 149 - point.y) -> Right
        // face 3
        else if (point.x == 49 && point.y >= 50) Point(point.y - 50, 100) -> Down
        //face 5
        else if (point.x == -1 && point.y < 150) Point(50, math.abs(149 - point.y)) -> Right
        //face 6
        else if (point.x == -1 && point.y >= 150) Point(point.y - 100, 0) -> Down
        else point -> facing
      case Right =>
        //face 2
        if (point.x == 150 && point.y < 50) Point(99, 149 - point.y) -> Left
        //face 3
        else if (point.x == 100 && point.y >= 50 && point.y < 100) Point(point.y + 50, 49) -> Up
        //face 4
        else if (point.x == 100 && point.y >= 100) Point(149, math.abs(point.y - 149)) -> Left
        //face 6
        else if (point.x == 50 && point.y >= 150) Point(point.y - 100, 149) -> Up
        else point -> facing
    }
  }
}
