package aoc2020

import aoc2020.InputGetter._

import scala.annotation.tailrec
import scala.collection.View

object Day08 extends AocTools(8) {
  val instructions: Seq[Instruction] = inputLines(Live)
    .map(parseLine)

  private def parseLine(l: String): Instruction = {
    val split = l.split(' ')
    split(0) match {
      case "nop" => Nop(split(1).toInt)
      case "acc" => Acc(split(1).toInt)
      case "jmp" => Jmp(split(1).toInt)
    }
  }

  sealed trait Instruction {
    def interpret(state: State): State = {
      this match {
        case Nop(_)   => state.copy(pointer = state.pointer + 1)
        case Acc(arg) => state.copy(pointer = state.pointer + 1, acc = state.acc + arg)
        case Jmp(arg) => state.copy(pointer = state.pointer + arg, acc = state.acc)
      }
    }
  }
  case class Nop(arg: Int) extends Instruction
  case class Acc(arg: Int) extends Instruction
  case class Jmp(arg: Int) extends Instruction

  sealed trait Result
  case class Looped(result: Int) extends Result
  case class Terminated(result: Int) extends Result

  case class State(pointer: Int, acc: Int)
  case object State {
    val empty: State = State(0, 0)
  }

  case class Program(instructions: Seq[Instruction]) {
    @tailrec
    final def run(state: State = State.empty, history: Set[Int] = Set.empty): Result = {
      state match {
        case State(pointer, acc) if pointer == instructions.size => Terminated(acc)
        case State(pointer, acc) if history.contains(pointer)    => Looped(acc)
        case State(pointer, _)                                   => run(instructions(pointer).interpret(state), history + pointer)
      }
    }
  }

  def permutations(input: Seq[Instruction]): View[Seq[Instruction]] = {
    input.zipWithIndex.view.collect {
      case (Jmp(argument), i) => input.updated(i, Nop(argument))
      case (Nop(argument), i) => input.updated(i, Jmp(argument))
    }
  }

  def main(args: Array[String]): Unit = {
    println(s"AOC 2020 - Day $day")

    val part1 = Program(instructions).run()
    val part2 = permutations(instructions).map(Program(_).run()).collectFirst { case res: Terminated => res }

    println(s"Answer part 1: $part1")
    println(s"Answer part 2: $part2")
  }
}
