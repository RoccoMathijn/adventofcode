package aoc2019

import scala.io.Source

object Day05 extends App {
  val input: List[Int] = Source
    .fromResource("aoc2019/input-day5.txt")
    .getLines()
    .mkString
    .split(',')
    .map(_.toInt)
    .toList

  def pMode(opcode: Int): (Int, Int, Int, Int) = {
    val padded: String = "%05d".format(opcode)
    (
      padded.takeRight(2).toInt,
      padded.substring(2, 3).toInt,
      padded.substring(1, 2).toInt,
      padded.substring(0, 1).toInt
    )
  }

  def getValue(index: Int, instructions: List[Int], parameterMode: Int): Int = {
    if (parameterMode == 0) instructions(instructions(index))
    else instructions(index)
  }

  def computer(instructions: List[Int], instructionPointer: Int, input: Int): List[Int] = {
    pMode(instructions(instructionPointer)) match {
      case (1, p1, p2, _) =>
        computer(
          instructions = instructions.updated(
            instructions(instructionPointer + 3),
            getValue(instructionPointer + 1, instructions, p1) + getValue(
              instructionPointer + 2,
              instructions,
              p2
            )
          ),
          instructionPointer = instructionPointer + 4,
          input = input
        )
      case (2, p1, p2, _) =>
        computer(
          instructions = instructions.updated(
            instructions(instructionPointer + 3),
            getValue(instructionPointer + 1, instructions, p1) * getValue(
              instructionPointer + 2,
              instructions,
              p2
            )
          ),
          instructionPointer = instructionPointer + 4,
          input = input
        )
      case (3, _, _, _) =>
        computer(
          instructions =
            instructions.updated(instructions(instructionPointer + 1), input),
          instructionPointer = instructionPointer + 2,
          input = input
        )
      case (4, p1, _, _) =>
        println(getValue(instructionPointer + 1, instructions, p1))
        computer(
          instructions = instructions,
          instructionPointer = instructionPointer + 2,
          input = input
        )
      case (5, p1, p2, _) =>
        computer(
          instructions =
            if (getValue(instructionPointer + 1, instructions, p1) != 0)
              instructions
                .updated(
                  instructionPointer,
                  getValue(instructionPointer + 2, instructions, p2)
                )
            else
              instructions,
          instructionPointer =
            if (getValue(instructionPointer + 1, instructions, p1) != 0)
              getValue(instructionPointer + 2, instructions, p2)
            else instructionPointer + 3,
          input = input
        )
      case (6, p1, p2, _) =>
        computer(
          instructions =
            if (getValue(instructionPointer + 1, instructions, p1) == 0)
              instructions
                .updated(
                  instructionPointer,
                  getValue(instructionPointer + 2, instructions, p2)
                )
            else
              instructions,
          instructionPointer =
            if (getValue(instructionPointer + 1, instructions, p1) == 0)
              getValue(instructionPointer + 2, instructions, p2)
            else instructionPointer + 3,
          input = input
        )
      case (7, p1, p2, _) =>
        computer(
          instructions =
            if (getValue(instructionPointer + 1, instructions, p1) < getValue(
                  instructionPointer + 2,
                  instructions,
                  p2
                ))
              instructions.updated(instructions(instructionPointer + 3), 1)
            else
              instructions
                .updated(instructions(instructionPointer + 3), 0),
          instructionPointer = instructionPointer + 4,
          input = input
        )
      case (8, p1, p2, _) =>
        computer(
          instructions =
            if (getValue(instructionPointer + 1, instructions, p1) == getValue(
                  instructionPointer + 2,
                  instructions,
                  p2
                ))
              instructions.updated(instructions(instructionPointer + 3), 1)
            else
              instructions.updated(instructions(instructionPointer + 3), 0),
          instructionPointer = instructionPointer + 4,
          input = input
        )
      case (99, _, _, _) => instructions
      case _                => throw new Error("Something went wrong!")
    }
  }

  def run(instructions: List[Int], input: Int) =
    computer(instructions, 0, input)

  run(input, 5)

}
