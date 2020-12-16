package aoc2020

import util.AocTools
import util.InputGetter.{Example, Live, Mode}

import scala.annotation.tailrec
import scala.util.matching.Regex

object Day16 extends AocTools(16, 2020) {
//  implicit private val mode: Mode = Example
  implicit private val mode: Mode = Live

  val input: Array[String] = inputBlob.split("\n\n")
  val rulesRegex: Regex = "([\\w ]+): (\\d+)-(\\d+) or (\\d+)-(\\d+)".r
  val rules: Array[Rule] = input.head.split('\n').map {
    case rulesRegex(name, r1From, r1To, r2From, r2Until) => Rule(name, r1From.toInt to r1To.toInt, r2From.toInt to r2Until.toInt)
  }
  case class Rule(name: String, r1: Range, r2: Range)

  val nearByTickets: Seq[List[Int]] = input.last.split('\n').drop(1).map(_.split(',').map(_.toInt).toList).toList
  val nearbyValidTickets: List[List[Int]] = nearByTickets.filter { ticket =>
    invalidValuesForTicket(ticket).isEmpty
  }.toList

  val myTicket: Seq[Int] = input(1).split('\n').drop(1).head.split(',').map(_.toInt).toList
  def possibleRulesPerPosition(validTickets: List[List[Int]]): Seq[(List[Rule], Int)] = {
    val fieldPositions = validTickets.transpose.zipWithIndex
    fieldPositions.map {
      case (values, position) =>
        val possibleRulesForPosition = rules.filter { rule =>
          values.forall(value => rule.r1.contains(value) || rule.r2.contains(value))
        }.toList
        possibleRulesForPosition -> position
    }
  }

  @tailrec
  def solveConstraints(input: Seq[(List[Rule], Int)], output: List[(Rule, Int)]): List[(Rule, Int)] = {
    if (input.isEmpty) output
    else {
      val (singleRules, multipleRules) = input.partition(_._1.size == 1)

      val singleRulesCleaned = singleRules.map(x => x._1.head -> x._2)
      val newOutput = output ++ singleRulesCleaned

      val multipleRulesFiltered = multipleRules.map {
        case (rules, i) => rules.filterNot(rule => newOutput.map(_._1).contains(rule)) -> i
      }
      solveConstraints(multipleRulesFiltered, newOutput)
    }
  }

  def invalidValuesForTicket(ticket: List[Int]): Seq[Int] = {
    ticket.filter { value =>
      !rules.exists(rule => rule.r1.contains(value) || rule.r2.contains(value))
    }
  }

  def getRuleName(position: Int, positionRules: List[(Rule, Int)]): String = {
    positionRules.find(_._2 == position).get._1.name
  }

  def main(args: Array[String]): Unit = {
    println(s"AOC 2020 - Day $day")
    val start = System.currentTimeMillis()

    val part1 = nearByTickets.flatMap(invalidValuesForTicket).sum
    val mid = System.currentTimeMillis()

    println(s"Answer part 1: $part1 [${mid - start}ms]")

    val possibleRulesForPosition = possibleRulesPerPosition(nearbyValidTickets)
    val positionPerRule: List[(Rule, Int)] = solveConstraints(possibleRulesForPosition, List.empty)

    val part2 = myTicket.zipWithIndex.collect {
      case (value, position) if getRuleName(position, positionPerRule).startsWith("departure") => value.toLong
    }.product

    val end = System.currentTimeMillis()
    println(s"Answer part 2: $part2 [${`end` - mid}ms]")
  }
}
