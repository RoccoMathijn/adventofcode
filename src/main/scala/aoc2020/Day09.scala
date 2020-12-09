package aoc2020

import aoc2020.InputGetter._

object Day09 extends AocTools(9) {
//  implicit private val mode: Mode = Example
  implicit private val mode: Mode = Live
  val preambleLength = 25

  val input = inputLongs

  def sumNext(target: Long, contigious: Seq[Long], pointer: Int): Option[Seq[Long]] = {
    if (contigious.sum == target) Some(contigious)
    else if (contigious.sum > target) None
    else sumNext(target, contigious :+ input(pointer), pointer + 1)
  }

  def main(args: Array[String]): Unit = {
    println(s"AOC 2020 - Day $day")

    val part1 = input.view.zipWithIndex
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
      .map(sumNext(part1.get, Seq.empty, _))
      .find(_.isDefined)
      .flatMap(_.map { contigious =>
        contigious.min + contigious.max
      })

    println(s"Answer part 2: $part2")
  }
}
