package aoc2015

import util.InputGetter.Live
import util.{AocTools, InputGetter}

object Day05 extends AocTools(5, 2015) {
  implicit val mode: InputGetter.Mode = Live
  val input: Seq[String] = inputLines

  val vowels = Set('a', 'e', 'i', 'o', 'u')

  def atLeastThreeVowels(string: String): Boolean = string.count(vowels.contains) >= 3
  def twiceInARow(string: String): Boolean = string.toList.sliding(2, 1).exists(list => list.head == list(1))
  def notContains(string: String): Boolean = !(string.contains("ab") || string.contains("cd") || string.contains("pq") || string.contains("xy"))

  def atLeastTwiceWithoutOverlapping(string: String): Boolean = {
    string.toList.sliding(2, 1).fold(List.empty[List[Char]])((acc, pair) => if (acc.headOption.contains(pair)) acc else pair :: acc).groupBy(identity).exists(_._2.size > 1)
  }

  def atLeastOneWhichRepeatsWithExactlyOneLetterInBetweenThem(string: String): Boolean = string.toList.sliding(3, 1).exists { case List(one, _, three) => one == three }

  def main(args: Array[String]): Unit = {
    val start = System.currentTimeMillis()
    println(s"AOC 2015 - Day $day")

    val part1 = input.count(string => atLeastThreeVowels(string) && twiceInARow(string) && notContains(string))
    val mid = System.currentTimeMillis()
    println(s"Answer part 1: $part1 [${mid - start}ms]")

    val part2 = input.count(string => atLeastTwiceWithoutOverlapping(string) && atLeastOneWhichRepeatsWithExactlyOneLetterInBetweenThem(string))
    val end = System.currentTimeMillis()
    println(s"Answer part 2: $part2 [${end - mid}ms]")
  }
}
