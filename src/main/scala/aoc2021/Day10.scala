package aoc2021

import util.AocTools
import util.InputGetter.{Live, Mode}

object Day10 extends AocTools(10, 2021) {
//  implicit private val mode: Mode = Example
  implicit private val mode: Mode = Live

  val input = inputLines

  private val emptyCounters = Map('}' -> 0, ']' -> 0, '>' -> 0, ')' -> 0)

  case class State(invalidChar: Option[Char], counters: Map[Char, Int], remainder: String)
  case object State {
    val empty = State(None, emptyCounters, "")
  }

  def begin(string: String): State = {
    findIllegal(
      string = string.tail,
      state = State.empty.copy(counters = emptyCounters.updatedWith(pairs(string.head))(_.map(_ + 1)))
    )
  }

  def findIllegal(string: String, state: State): State = {
    if (state.counters.exists(_._2 < 0)) state.copy(invalidChar = state.counters.find(_._2 < 0).map(_._1))
    else if (state.counters.forall(_._2 == 0)) state.copy(remainder = string)
    else if (string.isEmpty) state
    else {
      if (pairs.contains(string.head)) {
        val subState = begin(string)
        if (subState.invalidChar.isDefined) subState
        else findIllegal(subState.remainder, state)
      } else findIllegal(string.tail, state.copy(counters = state.counters.updatedWith(string.head)(_.map(_ - 1))))

    }
  }

  val charValueMap = Map(
    ')' -> 3,
    ']' -> 57,
    '}' -> 1197,
    '>' -> 25137
  )

  val pairs = Map(
    '(' -> ')',
    '[' -> ']',
    '{' -> '}',
    '<' -> '>'
  )

  val charValueMap2 = Map(
    ')' -> 1,
    ']' -> 2,
    '}' -> 3,
    '>' -> 4
  )

  def open(string: String): String = {
    string.foldLeft("")((acc, c) => {
      if (pairs.contains(c)) acc :+ c
      else acc.dropRight(1)
    })
  }

  def calculate(string: String): Long = {
    string.foldLeft(0L)((acc, c) => acc * 5 + charValueMap2(c))
  }

  def main(args: Array[String]): Unit = {
    val start = System.currentTimeMillis()
    println(s"AOC $year - Day $day")

    val part1 = input.map(line => begin(line)).flatMap(_.invalidChar).map(charValueMap).sum
    val mid = System.currentTimeMillis()
    println(s"Answer part 1: $part1 [${mid - start}ms]")

    val res = input.filterNot(line => begin(line).invalidChar.isDefined).map(line => open(line)).map(_.reverse.map(pairs)).map(calculate).sorted
    val part2 = res(res.size / 2)
    val end = System.currentTimeMillis()
    println(s"Answer part 2: $part2 [${end - mid}ms]")
  }
}
