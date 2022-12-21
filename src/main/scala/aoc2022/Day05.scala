package aoc2022

import util.AocTools
import util.InputGetter.{Example, Live, Mode}

object Day05 extends AocTools(5, 2022) {
//  implicit private val mode: Mode = Example
  implicit private val mode: Mode = Live

  val stacks: List[List[Char]] =
    inputLines.takeWhile(line => !line.startsWith(" 1")).transpose.filterNot(list => list.contains('[') || list.contains(']') || list.forall(_ == ' ')).map(_.filterNot(_ == ' '))

  val rearrangementProcedure: List[Instruction] =
    inputLines
      .dropWhile(_.nonEmpty)
      .drop(1)
      .map { case s"move $amount from $from to $to" => Instruction(amount.toInt, from.toInt - 1, to.toInt - 1) }

  case class Instruction(amount: Int, from: Int, to: Int)

  def rearrange(instructions: List[Instruction], stacks: List[List[Char]]): List[List[Char]] = {
    if (instructions.isEmpty) stacks
    else if (instructions.head.amount == 0) rearrange(instructions.tail, stacks)
    else {
      instructions.head match {
        case Instruction(amount, from, to) =>
          val newFrom = stacks(from).tail
          val newTo = stacks(from).head :: stacks(to)
          rearrange(Instruction(amount - 1, from, to) :: instructions.tail, stacks.updated(from, newFrom).updated(to, newTo))
      }
    }

  }

  def rearrange2(instructions: List[Instruction], stacks: List[List[Char]]): List[List[Char]] = {
    if (instructions.isEmpty) stacks
    else {
      instructions.head match {
        case Instruction(amount, from, to) =>
          val newFrom = stacks(from).drop(amount)
          val newTo = stacks(from).take(amount) ++ stacks(to)
          rearrange2(instructions.tail, stacks.updated(from, newFrom).updated(to, newTo))
      }
    }

  }

  def part1 = rearrange(rearrangementProcedure, stacks).map(_.head).mkString

  def part2 = rearrange2(rearrangementProcedure, stacks).map(_.head).mkString

  def main(args: Array[String]): Unit = {
    val start = System.currentTimeMillis()
    println(s"AOC 2022 - Day $day")

    val mid = System.currentTimeMillis()
    println(s"Answer part 1: $part1 [${mid - start}ms]")
    val end = System.currentTimeMillis()
    println(s"Answer part 2: $part2 [${end - mid}ms]")
  }
}
