package aoc2020

import util.AocTools
import util.InputGetter.{Example, Live, Mode}

import scala.annotation.tailrec
import scala.util.matching.Regex

object Day18 extends AocTools(18, 2020) {
//  implicit private val mode: Mode = Example
  implicit private val mode: Mode = Live

  val DigitR: Regex = "\\d+".r
  val input: Seq[String] = inputLines

  sealed trait Token
  case class Number(value: Long) extends Token
  case object Addition extends Token
  case object Multiplication extends Token
  case object OpenBracket extends Token
  case object ClosingBracket extends Token

  def tokenize(line: String): List[Token] = {
    line.replaceAll(" ", "").flatMap {
      case i if DigitR.matches(i.toString) => List(Number(i.toString.toLong))
      case '('                             => List(OpenBracket)
      case ')'                             => List(ClosingBracket)
      case '+'                             => List(Addition)
      case '*'                             => List(Multiplication)
    }
  }.toList

  @tailrec
  def takeUntilClosingBracket(input: List[Token], bracketCounter: Int = 1, output: List[Token] = List.empty): (List[Token], List[Token]) = {
    if (bracketCounter == 0) (output.init, input)
    else
      input.head match {
        case OpenBracket    => takeUntilClosingBracket(input.tail, bracketCounter + 1, output :+ OpenBracket)
        case ClosingBracket => takeUntilClosingBracket(input.tail, bracketCounter - 1, output :+ ClosingBracket)
        case other          => takeUntilClosingBracket(input.tail, bracketCounter, output :+ other)
      }
  }

  def takeUntilOpeningBracket(input: List[Token], bracketCounter: Int = 1, output: List[Token] = List.empty): (List[Token], List[Token]) = {
    if (bracketCounter == 0) (output.init, input)
    else
      input.head match {
        case OpenBracket    => takeUntilOpeningBracket(input.tail, bracketCounter - 1, output :+ OpenBracket)
        case ClosingBracket => takeUntilOpeningBracket(input.tail, bracketCounter + 1, output :+ ClosingBracket)
        case other          => takeUntilOpeningBracket(input.tail, bracketCounter, output :+ other)
      }
  }

  def insertOpeningBracket(acc: List[Token]): List[Token] = {
    val (part1, part2) = takeUntilOpeningBracket(acc.reverse)
    part2.reverse ++ List(OpenBracket, OpenBracket) ++ part1.reverse
  }

  def insertClosingBracket(remainder: List[Token]): List[Token] = {
    val (part1, part2) = takeUntilClosingBracket(remainder)
    part1 ++ List(ClosingBracket, ClosingBracket) ++ part2
  }

  def solve(tokens: List[Token]): Long = {
    tokens match {
      case Number(value) :: Nil                                  => value
      case OpenBracket :: xs                                     => solve(Number(solve(takeUntilClosingBracket(xs)._1)) :: takeUntilClosingBracket(xs)._2)
      case Number(left) :: Addition :: OpenBracket :: xs         => solve(Number(left + solve(takeUntilClosingBracket(xs)._1)) :: takeUntilClosingBracket(xs)._2)
      case Number(left) :: Multiplication :: OpenBracket :: xs   => solve(Number(left * solve(takeUntilClosingBracket(xs)._1)) :: takeUntilClosingBracket(xs)._2)
      case Number(left) :: Addition :: Number(right) :: xs       => solve(Number(left + right) :: xs)
      case Number(left) :: Multiplication :: Number(right) :: xs => solve(Number(left * right) :: xs)
    }
  }

  def parenthesize(remainder: List[Token], acc: List[Token] = List.empty): List[Token] = {
    remainder match {
      case Number(left) :: Addition :: Number(right) :: xs =>
        parenthesize(remainder = ClosingBracket :: xs, acc = acc ++ List(OpenBracket, Number(left), Addition, Number(right)))

      case Number(left) :: Addition :: OpenBracket :: xs =>
        val newOutput = acc ++ List(OpenBracket, Number(left), Addition, OpenBracket)
        parenthesize(remainder = insertClosingBracket(xs), acc = newOutput)

      case ClosingBracket :: Addition :: Number(right) :: xs =>
        val end = List(ClosingBracket, Addition, Number(right))
        parenthesize(remainder = ClosingBracket :: xs, acc = insertOpeningBracket(acc) ++ end)

      case ClosingBracket :: Addition :: OpenBracket :: xs =>
        val accWithOpeningBracket = insertOpeningBracket(acc)
        val remainderWithClosingBracket = insertClosingBracket(xs)
        parenthesize(remainderWithClosingBracket, accWithOpeningBracket ++ List(ClosingBracket, Addition, OpenBracket))

      case x :: xs => parenthesize(xs, acc :+ x)
      case Nil     => acc
    }
  }

  def show(input: List[Token]) {
    println(
      input.map {
        case Number(value)  => value.toString
        case Addition       => " + "
        case Multiplication => " * "
        case OpenBracket    => "("
        case ClosingBracket => ")"
      }.mkString + s" becomes ${solve(input)}"
    )
  }

  def main(args: Array[String]): Unit = {
    println(s"AOC 2020 - Day $day")
    val start = System.currentTimeMillis()

    val part1 = input.map(tokenize).map(solve).sum
    val mid = System.currentTimeMillis()

    println(s"Answer part 1: $part1 [${mid - start}ms]")

    val newForms = input.map(tokenize).map(parenthesize(_))
    val part2 = newForms.map(solve).sum
    val end = System.currentTimeMillis()
    println(s"Answer part 2: $part2 [${`end` - mid}ms]")

  }
}
