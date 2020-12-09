package aoc2020

import aoc2020.InputGetter._

object Day09 extends AocTools(9) {
//  implicit private val mode: Mode = Example
  implicit private val mode: Mode = Live

  private val input = inputLongs
  private val preambleLength = mode match {
    case InputGetter.Live    => 25
    case InputGetter.Example => 5
  }

  def findContigious(target: Long, sequence: Seq[Long], pointer: Int): Option[Seq[Long]] = {
    if (sequence.sum == target) Some(sequence)
    else if (sequence.sum > target) None
    else findContigious(target, sequence :+ input(pointer), pointer + 1)
  }

  def main(args: Array[String]): Unit = {
    println(s"AOC 2020 - Day $day")

    val part1: Option[Long] = input.view.zipWithIndex
      .drop(preambleLength)
      .find { x =>
        val preamble = input.slice(x._2 - preambleLength, x._2)
        val sums = preamble.flatMap { y =>
          preamble.map { x =>
            x + y
          }
        }
        !sums.contains(x._1)
      }
      .map(_._1)

    println(s"Answer part 1: $part1")

    val part2: Option[Long] = input.indices
      .map { findContigious(part1.get, Seq.empty, _) }
      .collectFirst { case Some(contigious) => contigious.min + contigious.max }

    println(s"Answer part 2: $part2")
  }
}
