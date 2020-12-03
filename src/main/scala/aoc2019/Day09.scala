package aoc2019

import scala.io.Source
import scala.util.Try

object Day09 extends App {
  val input: List[Long] = Source
    .fromResource("aoc2019/input-day9.txt")
    .getLines()
    .mkString
    .split(',')
    .map(_.toLong)
    .toList

    def pMode(opcode: Long): (Int, Int, Int, Int) = {
      val padded: String = "%05d".format(opcode)
      (
        padded.takeRight(2).toInt,
        padded.substring(2, 3).toInt,
        padded.substring(1, 2).toInt,
        padded.substring(0, 1).toInt
      )
    }

    def getValue(index: Int, instructions: List[Long], parameterMode: Int, relativeBase: Int): Long = {
      println(s"getValue: index: $index")
      if (parameterMode == 0) Try(instructions(instructions(index).toInt)).getOrElse(0)
      else if(parameterMode == 1) Try(instructions(index)).getOrElse(0)
      else  Try(instructions(relativeBase + (instructions(index)).toInt)).getOrElse(0)
    }

    def computer(instructions: List[Long], instructionPointer: Int, phaseSetting: Long, input: Long, output: Long, relativeBase: Int): (List[Long], Int, Long, Long, Long, Int) = {
      println(s"Instructions: $instructions, pointer: $instructionPointer, phaseSetting: $phaseSetting, input: $input, output: $output, relativeBase: $relativeBase")
      pMode(instructions(instructionPointer)) match {
        case (1, p1, p2, _) =>
          computer(
            instructions = instructions.updated(
              instructions(instructionPointer + 3).toInt,
              getValue(instructionPointer + 1, instructions, p1, relativeBase) + getValue(
                instructionPointer + 2,
                instructions,
                p2,
                relativeBase
              )
            ),
            instructionPointer = instructionPointer + 4,
            phaseSetting = phaseSetting,
            input = input,
            output = output,
            relativeBase
          )
        case (2, p1, p2, _) =>
          computer(
            instructions = instructions.updated(
              instructions(instructionPointer + 3).toInt,
              getValue(instructionPointer + 1, instructions, p1, relativeBase) * getValue(
                instructionPointer + 2,
                instructions,
                p2,
                relativeBase
              )
            ),
            instructionPointer = instructionPointer + 4,
            phaseSetting = phaseSetting,
            input = input,
            output = output,
            relativeBase
          )
        case (3, _, _, _) =>
          computer(
            instructions =
              instructions.updated(instructions(instructionPointer + 1).toInt, phaseSetting),
            instructionPointer = instructionPointer + 2,
            phaseSetting = input,
            input = input,
            output = output,
            relativeBase
          )
        case (4, p1, _, _) =>
          val newOutput = getValue(instructionPointer + 1, instructions, p1, relativeBase)
          println(s"Output: $newOutput")
//          (instructions, instructionPointer + 2, phaseSetting, input, newOutput, relativeBase)
          computer(instructions, instructionPointer + 2, phaseSetting, input, newOutput, relativeBase)
        case (5, p1, p2, _) =>
          computer(
            instructions =
              if (getValue(instructionPointer + 1, instructions, p1, relativeBase) != 0)
                instructions
                  .updated(
                    instructionPointer,
                    getValue(instructionPointer + 2, instructions, p2, relativeBase)
                  )
              else
                instructions,
            instructionPointer =
              if (getValue(instructionPointer + 1, instructions, p1, relativeBase) != 0)
                getValue(instructionPointer + 2, instructions, p2, relativeBase).toInt
              else instructionPointer + 3,
            phaseSetting = phaseSetting,
            input = input,
            output = output,
            relativeBase
          )
        case (6, p1, p2, _) =>
          computer(
            instructions =
              if (getValue(instructionPointer + 1, instructions, p1, relativeBase) == 0)
                instructions
                  .updated(
                    instructionPointer,
                    getValue(instructionPointer + 2, instructions, p2, relativeBase)
                  )
              else
                instructions,
            instructionPointer =
              if (getValue(instructionPointer + 1, instructions, p1, relativeBase) == 0)
                getValue(instructionPointer + 2, instructions, p2, relativeBase).toInt
              else instructionPointer + 3,
            phaseSetting = phaseSetting,
            input = input,
            output = output,
            relativeBase
          )
        case (7, p1, p2, _) =>
          computer(
            instructions =
              if (getValue(instructionPointer + 1, instructions, p1, relativeBase) < getValue(
                instructionPointer + 2,
                instructions,
                p2,
                relativeBase
              ))
                instructions.updated(instructions(instructionPointer + 3).toInt, 1l)
              else
                instructions.updated(instructions(instructionPointer + 3).toInt, 0l),
            instructionPointer = instructionPointer + 4,
            phaseSetting = phaseSetting,
            input = input,
            output = output,
            relativeBase
          )
        case (8, p1, p2, _) =>
          computer(
            instructions =
              if (getValue(instructionPointer + 1, instructions, p1, relativeBase) == getValue(
                instructionPointer + 2,
                instructions,
                p2,
                relativeBase
              ))
                instructions.updated(instructions(instructionPointer + 3).toInt, 1l)
              else
                instructions.updated(instructions(instructionPointer + 3).toInt, 0l),
            instructionPointer = instructionPointer + 4,
            phaseSetting = phaseSetting,
            input = input,
            output = output,
            relativeBase
          )
        case (9, p1, _, _) =>
          println(s"Relative base: ${relativeBase + getValue(instructionPointer + 1, instructions, p1, relativeBase).toInt}")
          computer(
            instructions = instructions,
            instructionPointer = instructionPointer + 2,
            phaseSetting = phaseSetting,
            input = input,
            output = output,
            relativeBase + getValue(instructionPointer + 1, instructions, p1, relativeBase).toInt
          )
        case (99, _, _, _) => throw new Error("Halt")
        case _                => throw new Error("Something went wrong!")
      }
    }

    def run() = {
      computer(input, 0, 0, 0, 0, 0)
    }

  println(run())
}
