package aoc2022

import util.AocTools
import util.InputGetter.{Example, Live, Mode}

object Day2 extends AocTools(2, 2022) {
//    implicit private val mode: Mode = Example
  implicit private val mode: Mode = Live

  private val input: List[Array[String]] = inputLines.map(_.split(" "))
  val roundsPart1 = input.map(ar => Round(parseShape(ar.head), parseShape(ar.last)))
  val roundsPart2 = input.map(ar => {
    val them = parseShape(ar.head)
    Round(them, chooseShape(them, parseDesiredOutcome(ar.last)))
  })

  def parseShape(value: String): Shape =
    value match {
      case "A" | "X" => Rock
      case "B" | "Y" => Paper
      case "C" | "Z" => Scissors
    }

  def parseDesiredOutcome(value: String): Outcome =
    value match {
      case "X" => Lose
      case "Y" => Draw
      case "Z" => Win
    }

  lazy val beats: Map[Shape, Shape] = Map(
    Rock -> Scissors,
    Scissors -> Paper,
    Paper -> Rock
  )

  def chooseShape(them: Shape, desiredOutcome: Outcome): Shape =
    desiredOutcome match {
      case Lose => beats(them)
      case Draw => them
      case Win  => beats.groupMap(_._2)(_._1)(them).head
    }

  sealed trait Outcome
  case object Lose extends Outcome
  case object Draw extends Outcome
  case object Win extends Outcome

  case class Round(them: Shape, us: Shape)

  sealed trait Shape
  case object Rock extends Shape
  case object Paper extends Shape
  case object Scissors extends Shape

  val shapePoint: Map[Shape, Int] = Map(
    Rock -> 1,
    Paper -> 2,
    Scissors -> 3
  )

  def play(round: Round): Int = {
    if (beats(round.them) == round.us) 0
    else if (round.them == round.us) 3
    else 6
  }

  def points(round: Round): Int = play(round) + shapePoint(round.us)

  val part1 = roundsPart1.map(points).sum

  val part2 = roundsPart2.map(points).sum

  def main(args: Array[String]): Unit = {
    val start = System.currentTimeMillis()
    println(s"AOC 2022 - Day $day")

    val mid = System.currentTimeMillis()
    println(s"Answer part 1: $part1 [${mid - start}ms]")
    val end = System.currentTimeMillis()
    println(s"Answer part 2: $part2 [${end - mid}ms]")
  }
}
