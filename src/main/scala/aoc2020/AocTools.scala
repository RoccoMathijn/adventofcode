package aoc2020

import aoc2020.InputGetter.Mode

abstract class AocTools(val day: Int) {

  def inputLines(implicit mode: Mode): List[String] = InputGetter.get(day, mode).toList

  def inputInts(implicit mode: Mode): Seq[Int] = inputLines(mode).map(_.toInt)

  def inputLongs(implicit mode: Mode): Seq[Long] = inputLines(mode).map(_.toLong)

  def inputBlob(implicit mode: Mode): String = inputLines(mode).mkString("\n")
}
