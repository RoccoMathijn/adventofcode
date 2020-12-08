package aoc2020

import aoc2020.InputGetter.Mode

abstract class AocTools(val day: Int) {

  def inputLines(mode: Mode): List[String] = InputGetter.get(day, mode).toList

  def inputInts(mode: Mode): List[Int] = inputLines(mode).map(_.toInt)

  def inputBlob(mode: Mode) = inputLines(mode).mkString("\n")
}
