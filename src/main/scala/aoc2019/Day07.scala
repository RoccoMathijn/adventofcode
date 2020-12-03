package aoc2019

import scala.io.Source

object Day07 extends App {
  val instructions: List[Int] = Source
    .fromResource("aoc2019/input-day7.txt")
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

  def computer(instructions: List[Int], instructionPointer: Int, phaseSetting: Int, input: Int, output: Int): (List[Int], Int, Int, Int, Int) = {
    println(s"Instructions: $instructions, pointer: $instructionPointer, phaseSetting: $phaseSetting, input: $input, output: $output")
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
          phaseSetting = phaseSetting,
          input = input,
          output = output
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
          phaseSetting = phaseSetting,
          input = input,
          output = output
        )
      case (3, _, _, _) =>
        computer(
          instructions =
            instructions.updated(instructions(instructionPointer + 1), phaseSetting),
          instructionPointer = instructionPointer + 2,
          phaseSetting = input,
          input = input,
          output = output
        )
      case (4, p1, _, _) =>
        val newOutput = getValue(instructionPointer + 1, instructions, p1)
        println(s"Output: $newOutput")
        (instructions, instructionPointer + 2, phaseSetting, input, newOutput)
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
          phaseSetting = phaseSetting,
          input = input,
          output = output
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
          phaseSetting = phaseSetting,
          input = input,
          output = output
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
          phaseSetting = phaseSetting,
          input = input,
          output = output
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
          phaseSetting = phaseSetting,
          input = input,
          output = output
        )
      case (99, _, _, _) => throw new Error("Halt")
      case _                => throw new Error("Something went wrong!")
    }
  }

  def allPhaseSettingsPart1(): List[List[Int]] = List(0,1,2,3,4).permutations.toList
  def allPhaseSettingsPart2(): List[List[Int]] = List(5,6,7,8,9).permutations.toList

  def run(state: List[(List[Int], Int, Int, Int, Int)], firstRun: Boolean, phaseSettings: List[Int]): Int = {
    println("start amp1")
    val amplifier1 = computer(instructions = state(0)._1, instructionPointer = state(0)._2, phaseSetting = if (firstRun) phaseSettings(0) else state(4)._5, input = state(4)._5, output = 0)
    val amplifier2 = computer(instructions = state(1)._1, instructionPointer = state(1)._2, phaseSetting = if (firstRun) phaseSettings(1) else amplifier1._5, input = amplifier1._5, output = 0)
    val amplifier3 = computer(instructions = state(2)._1, instructionPointer = state(2)._2, phaseSetting = if (firstRun) phaseSettings(2) else amplifier2._5, input = amplifier2._5, output = 0)
    val amplifier4 = computer(instructions = state(3)._1, instructionPointer = state(3)._2, phaseSetting = if (firstRun) phaseSettings(3) else amplifier3._5, input = amplifier3._5, output = 0)
    val amplifier5 = computer(instructions = state(4)._1, instructionPointer = state(4)._2, phaseSetting = if (firstRun) phaseSettings(4) else amplifier4._5, input = amplifier4._5, output = 0)

    val instructionPointersList = List(amplifier1._2, amplifier2._2, amplifier3._2, amplifier4._2, amplifier5._2)
//    println(instructionPointersList)
//    println(s"State 5: $amplifier5")
    run(List(amplifier1, amplifier2, amplifier3, amplifier4, amplifier5), false, phaseSettings)
  }

//  println(allPhaseSettingsPart1().map(setting => run(setting, 0)).max)
//  println(allPhaseSettingsPart2().map(setting => run(setting, 0)).max)

  val initialState1 = (instructions, 0, 0, 0, 0)
  val initialState2 = (instructions, 0, 0, 0, 0)
  val initialState3 = (instructions, 0, 0, 0, 0)
  val initialState4 = (instructions, 0, 0, 0, 0)
  val initialState5 = (instructions, 0, 0, 0, 0)
  println(run(List(initialState1, initialState2, initialState3, initialState4, initialState5), true, phaseSettings = List(9,8,7,6,5)))
}
