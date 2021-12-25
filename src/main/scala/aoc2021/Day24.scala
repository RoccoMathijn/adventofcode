package aoc2021

import util.AocTools
import util.InputGetter.{Live, Mode}

object Day24 extends AocTools(24, 2021) {
//  implicit private val mode: Mode = Example
  implicit private val mode: Mode = Live

  sealed trait Instruction extends Product with Serializable
  case class Input(a: Char) extends Instruction
  case class Add(a: Char, b: String) extends Instruction
  case class Mul(a: Char, b: String) extends Instruction
  case class Div(a: Char, b: String) extends Instruction
  case class Mod(a: Char, b: String) extends Instruction
  case class Eql(a: Char, b: String) extends Instruction

  def parse(input: List[String]): List[Instruction] = {
    input.map {
      case s"inp $a"    => Input(a.head)
      case s"add $a $b" => Add(a.head, b)
      case s"mul $a $b" => Mul(a.head, b)
      case s"div $a $b" => Div(a.head, b)
      case s"mod $a $b" => Mod(a.head, b)
      case s"eql $a $b" => Eql(a.head, b)
    }
  }

  val startState: Map[Char, Long] = Map('w' -> 0L, 'x' -> 0L, 'y' -> 0L, 'z' -> 0L)
  val instructions: List[Instruction] = parse(inputLines)
  def valueOfB(b: String, state: Map[Char, Long]): Long = {
    if (startState.keys.toSet.contains(b.head)) state(b.head) else b.toInt
  }

  def eval(input: String, instructions: List[Instruction] = instructions, state: Map[Char, Long] = startState): Map[Char, Long] = {
    if (instructions.isEmpty) {
      println("\n")
      state
    }
    else {
      instructions.head match {
        case Input(a) =>
          val newState = state.updated(a, input.head.toString.toLong)
            println(newState)
          eval(input.tail, instructions.tail, newState)
        case Add(a, b) =>
          val l = state(a)
          val r = valueOfB(b, state)
          val newState = state.updated(a, l + r)
          eval(input, instructions.tail, newState)
        case Mul(a, b) =>
          val l = state(a)
          val r = valueOfB(b, state)
          val newState = state.updated(a, l * r)
          eval(input, instructions.tail, newState)
        case Div(a, b) =>
          val l = state(a)
          val r = valueOfB(b, state)
          val newState = state.updated(a, l / r)
          eval(input, instructions.tail, newState)
        case Mod(a, b) =>
          val l = state(a)
          val r = valueOfB(b, state)
          val newState = state.updated(a, l % r)
          eval(input, instructions.tail, newState)
        case Eql(a, b) =>
          val l = state(a)
          val r = valueOfB(b, state)
          val newA = if (l == r) 1L else 0L
          val newState = state.updated(a, newA)
          eval(input, instructions.tail, newState)
      }
    }
  }

  def isValid(state: Map[Char, Long]): Boolean = state('z') == 0
  
  def all: Seq[List[Int]] = {
    for {
      one <- 1 to 9
      two <- 1 to 9
      three <- 1 to 1
      four <- 1 to 1
      five <- 1 to 1
      six <- 1 to 1
      seven <- 1 to 1
      eight <- 1 to 1
      nine <- 1 to 1
      ten <- 1 to 1
      eleven <- 1 to 1
      twelve <- 1 to 1
      thirteen <- 1 to 1
      fourteen <- 1 to 1
    } yield List(one, two, three, four, five, six, seven, eight, nine, ten, eleven, twelve, thirteen, fourteen)
  }

  def findLargest: Long = {
    all.collect {
      case input if isValid(eval(input.map(_.toString).mkString)) =>
        val res = input.map(_.toString).mkString.toLong
        println(res)
        res
    }.min
  }
  def solve1 = findLargest
  def solve2 = ""

  def main(args: Array[String]): Unit = {
    val start = System.currentTimeMillis()
    println(s"AOC $year - Day $day")

    val part1 = solve1

    val mid = System.currentTimeMillis()
    println(s"Answer part 1: $part1 [${mid - start}ms]")

    val part2 = solve2
    val end = System.currentTimeMillis()
    println(s"Answer part 2: $part2 [${end - mid}ms]")
  }
}
