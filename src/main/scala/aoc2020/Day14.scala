package aoc2020

import util.AocTools
import util.InputGetter.{Example, Live, Mode}

import scala.annotation.tailrec
import scala.util.matching.Regex

object Day14 extends AocTools(14, 2020) {
//  implicit private val mode: Mode = Example
  implicit private val mode: Mode = Live

  val allocationPattern: Regex = "mem\\[(\\d+)\\] = (\\d+)".r
  val maskPattern: Regex = "mask = (\\w+)".r

  sealed trait Instruction
  case class Mask(value: String) extends Instruction
  case class Allocation(address: Int, value: Long) extends Instruction

  val input: Seq[Instruction] = inputLines.map {
    case maskPattern(mask)                     => Mask(mask)
    case allocationPattern(instruction, value) => Allocation(instruction.toInt, value.toLong)
  }

  def applyMask(mask: String, value: Long): Long = {
    mask.reverse.zipWithIndex.foldLeft(value) {
      case (newValue, ('X', _)) => newValue
      case (newValue, ('0', i)) => newValue & ~(1L << i)
      case (newValue, ('1', i)) => newValue | (1L << i)
    }
  }

  def applyMask2(mask: String, value: Long): Seq[Long] = {
    mask.reverse.zipWithIndex.foldLeft(Seq(value)) {
      case (newValues, ('X', i)) => newValues.flatMap(newValue => Seq(newValue & ~(1L << i), newValue | (1L << i)))
      case (newValues, ('0', _)) => newValues
      case (newValues, ('1', i)) => newValues.map(_ | (1L << i))
    }
  }

  def unsignedLong(a: Long): scala.math.BigInt = {
    (BigInt(a >>> 1) << 1) + (a & 1)
  }

  @tailrec
  def runProgram(input: Seq[Instruction], currMask: String, memory: Map[Long, Long], part2: Boolean): Map[Long, Long] =
    input match {
      case Nil               => memory
      case Mask(value) :: xs => runProgram(xs, value, memory, part2)
      case Allocation(address, value) :: xs =>
        val newMemory =
          if (part2)
            memory ++ applyMask2(currMask, address).map(_ -> value).toMap
          else
            memory + (address.toLong -> applyMask(currMask, value))
        runProgram(xs, currMask, newMemory, part2)
    }

  def main(args: Array[String]): Unit = {
    println(s"AOC 2020 - Day $day")

    val memory1 = runProgram(input, "", Map.empty[Long, Long], part2 = false)
    val part1: BigInt = memory1.values.map(unsignedLong).sum
    println(s"Answer part 1: $part1")

    val memory2 = runProgram(input, "", Map.empty[Long, Long], part2 = true)
    val part2 = memory2.values.map(unsignedLong).sum
    println(s"Answer part 2: $part2")
  }
}
