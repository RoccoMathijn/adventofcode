package util

import util.InputGetter.Mode

abstract class AocTools(val day: Int, val year: Int) {

  def inputLines(implicit mode: Mode): List[String] = InputGetter.get(day, year, mode).toList

  def inputInts(implicit mode: Mode): List[Int] = inputLines(mode).map(_.toInt)

  def inputLongs(implicit mode: Mode): List[Long] = inputLines(mode).map(_.toLong)

  def inputBlob(implicit mode: Mode): String = inputLines(mode).mkString("\n")
}
