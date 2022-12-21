package aoc2022

import util.AocTools
import util.InputGetter.{Example, Live, Mode}

object Day21 extends AocTools(21, 2022) {
//  implicit private val mode: Mode = Example
  implicit private val mode: Mode = Live
  sealed trait Job
  case class Constant(value: Long) extends Job
  case class Add(valueA: String, valueB: String) extends Job
  case class Subtract(valueA: String, valueB: String) extends Job
  case class Multiply(valueA: String, valueB: String) extends Job
  case class Divide(valueA: String, valueB: String) extends Job

  case class Monkey(name: String, job: Job)
  val input = inputLines.map {
    case s"$name: $valueA + $valueB" => Monkey(name, Add(valueA, valueB))
    case s"$name: $valueA - $valueB" => Monkey(name, Subtract(valueA, valueB))
    case s"$name: $valueA * $valueB" => Monkey(name, Multiply(valueA, valueB))
    case s"$name: $valueA / $valueB" => Monkey(name, Divide(valueA, valueB))
    case s"$name: $valueA"           => Monkey(name, Constant(valueA.toLong))
  }

  def eval(monkeyName: String): Long = {
    val monkey = input.find(_.name == monkeyName).get
    monkey match {
      case Monkey(_, Constant(x))              => x
      case Monkey(_, Add(valueA, valueB))      => eval(valueA) + eval(valueB)
      case Monkey(_, Subtract(valueA, valueB)) => eval(valueA) - eval(valueB)
      case Monkey(_, Multiply(valueA, valueB)) => eval(valueA) * eval(valueB)
      case Monkey(_, Divide(valueA, valueB))   => eval(valueA) / eval(valueB)
    }
  }

  def eval2(monkeyName: String, result: Option[Long] = None): Option[Long] = {
    val monkey = input.find(_.name == monkeyName).get
    monkey match {
      case Monkey("humn", _) => result
      case Monkey(_, Constant(x)) => Some(x)
      case Monkey(_, Add(valueA, valueB)) =>
        val a = eval2(valueA, None)
        val b = eval2(valueB, None)
        (result, a, b) match {
          case (_, Some(x), Some(y))      => Some(x + y)
          case (Some(res), None, Some(y)) => eval2(valueA, Some(res - y))
          case (Some(res), Some(x), None) => eval2(valueB, Some(res - x))
          case _ => None
        }
      case Monkey(_, Subtract(valueA, valueB)) =>
        val a = eval2(valueA, None)
        val b = eval2(valueB, None)
        (result, a, b) match {
          case (_, Some(x), Some(y))      => Some(x - y)
          case (Some(res), None, Some(y)) => eval2(valueA, Some(res + y))
          case (Some(res), Some(x), None) => eval2(valueB, Some(x - res))
          case _ => None
        }
      case Monkey(_, Multiply(valueA, valueB)) =>
        val a = eval2(valueA, None)
        val b = eval2(valueB, None)
        (result, a, b) match {
          case (_, Some(x), Some(y)) => Some(x * y)
          case (Some(res), None, Some(y)) => eval2(valueA, Some(res / y))
          case (Some(res), Some(x), None) => eval2(valueB, Some(res / x))
          case _ => None
        }
      case Monkey(_, Divide(valueA, valueB)) =>
        val a = eval2(valueA, None)
        val b = eval2(valueB, None)
        (result, a, b) match {
          case (_, Some(x), Some(y)) => Some(x / y)
          case (Some(res), None, Some(y)) => eval2(valueA, Some(res * y))
          case (Some(res), Some(x), None) => eval2(valueB, Some(x / res))
          case _ => None
        }
    }
  }

  def evalRoot: Option[Long] = {
    val root = input.find(_.name == "root").get
    root.job match {
      case Add(valueA, valueB) =>
        eval2(valueA, eval2(valueB, None))
    }
  }

  def solve1 = eval("root")
  def solve2 = evalRoot.get

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
