package aoc2020

import util.AocTools
import util.InputGetter.{Example, Live, Mode}

import scala.util.matching.Regex

object Day19 extends AocTools(19, 2020) {
  implicit private val mode: Mode = Example
//  implicit private val mode: Mode = Live

  type Rules = Map[Int, Rule]
  sealed trait Rule
  case class Match(string: String) extends Rule
  case class Reference(rule: Int) extends Rule
  case class And(rules: List[Rule]) extends Rule
  case class Or(rules: List[Rule]) extends Rule

  val MatchR: Regex = "(\\d+): \"(\\w)\"".r
  val SingleR: Regex = "(\\d+): (\\d+)".r
  val DoubleAndR: Regex = "(\\d+): (\\d+) (\\d+)".r
  val TripleAndR: Regex = "(\\d+): (\\d+) (\\d+) (\\d+)".r
  val SingleOrR: Regex = "(\\d+): (\\d+) \\| (\\d+)".r
  val SingleOrDoubleAnd: Regex = "(\\d+): (\\d+) \\| (\\d+) (\\d+)".r
  val DoubleAndOrTripleAnd: Regex = "(\\d+): (\\d+) (\\d+) \\| (\\d+) (\\d+) (\\d+)".r
  val DoubleOrR: Regex = "(\\d+): (\\d+) (\\d+) \\| (\\d+) (\\d+)".r

  val rules: Rules = inputBlob
    .split("\n\n")
    .head
    .split("\n")
    .map(parseLine)
    .toMap

  private def parseLine(line: String): (Int, Rule) =
    line match {
      case MatchR(number, char)                           => number.toInt -> Match(char)
      case SingleR(number, rule)                          => number.toInt -> Reference(rule.toInt)
      case DoubleAndR(number, rule1, rule2)               => number.toInt -> And(List(Reference(rule1.toInt), Reference(rule2.toInt)))
      case TripleAndR(number, rule1, rule2, rule3)        => number.toInt -> And(List(Reference(rule1.toInt), Reference(rule2.toInt), Reference(rule3.toInt)))
      case SingleOrR(number, rule1, rule2)                => number.toInt -> Or(List(Reference(rule1.toInt), Reference(rule2.toInt)))
      case SingleOrDoubleAnd(number, rule1, rule2, rule3) => number.toInt -> Or(List(Reference(rule1.toInt), And(List(Reference(rule2.toInt), Reference(rule3.toInt)))))
      case DoubleAndOrTripleAnd(number, rule1, rule2, rule3, rule4, rule5) =>
        number.toInt -> Or(List(And(List(Reference(rule1.toInt), Reference(rule2.toInt))), And(List(Reference(rule3.toInt), Reference(rule4.toInt), Reference(rule5.toInt)))))
      case DoubleOrR(number, rule1, rule2, rule3, rule4) =>
        number.toInt -> Or(List(And(List(Reference(rule1.toInt), Reference(rule2.toInt))), And(List(Reference(rule3.toInt), Reference(rule4.toInt)))))
    }

  lazy val rules2 = ((rules.updated _).tupled(parseLine("8: 42 | 42 8")).updated _).tupled(parseLine("11: 42 31 | 42 11 31"))
  val messages: Seq[String] = inputBlob.split("\n\n").drop(1).head.split("\n").toList

  def resolve(rule: Rule, rules: Rules): List[String] = {
    rule match {
      case Match(string) => List(string)
      case Reference(number) =>
        val resolved = rules(number)
        resolve(resolved, rules)
      case And(ruleList) => concatLists(ruleList.map(resolve(_, rules)))
      case Or(ruleList)  => ruleList.flatMap(resolve(_, rules))
    }
  }

  def ruleZero(rules: Rules): Seq[String] = resolve(Reference(0), rules)

  def concatLists(lists: List[List[String]]): List[String] = {
    lists.foldLeft(List("")) { (acc, position) =>
      position.flatMap(string => acc.map(a => a + string))
    }
  }

  def isValid(message: String, rules: Rules) = ruleZero(rules).contains(message)

  def main(args: Array[String]): Unit = {
    println(s"AOC 2020 - Day $day")
    val start = System.currentTimeMillis()

    val part1 = messages.count(isValid(_, rules))
    val mid = System.currentTimeMillis()

    println(s"Answer part 1: $part1 [${mid - start}ms]")

    val part2 = messages.count(isValid(_, rules2))
    val end = System.currentTimeMillis()
    println(s"Answer part 2: $part2 [${`end` - mid}ms]")
  }
}
