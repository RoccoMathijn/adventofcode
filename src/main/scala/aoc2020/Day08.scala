package aoc2020

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.io.Source

object Day08 extends App {
  val input: Seq[Instruction] = Source
    .fromResource("aoc2020/input-day8.txt")
    .getLines()
    .toList
    .map { l =>
      val split = l.split(' ')
      Instruction(split(0), split(1).toInt)
    }

  case class Instruction(operation: String, argument: Int)

  @tailrec
  def run(input: Seq[Instruction], history: List[Int] = List.empty, pointer: Int = 0, acc: Int = 0): Either[Int, Int] = {
    if (pointer == input.size) Right(acc)
    else if (history.contains(pointer)) Left(acc)
    else {
      input(pointer).operation match {
        case "nop" => run(input, history :+ pointer, pointer + 1, acc)
        case "acc" => run(input, history :+ pointer, pointer + 1, acc + input(pointer).argument)
        case "jmp" => run(input, history :+ pointer, pointer + input(pointer).argument, acc)
      }
    }
  }

  def permutations(input: Seq[Instruction]): Seq[Seq[Instruction]] = {
    var permutations = new ListBuffer[Seq[Instruction]].empty
    for (i <- input.indices) {
      val instruction: Instruction = input(i)
      instruction.operation match {
        case "jmp" => permutations += input.patch(i, List(Instruction("nop", instruction.argument)), 1)
        case "nop" => permutations += input.patch(i, List(Instruction("jmp", instruction.argument)), 1)
        case _     =>
      }
    }
    permutations.toSeq
  }

  println(run(input))
  println(permutations(input).map(run(_)).filter(_.isRight))
}
