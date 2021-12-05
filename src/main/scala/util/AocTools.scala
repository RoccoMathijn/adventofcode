package util

import util.InputGetter.Mode

abstract class AocTools(val day: Int, val year: Int) {

  def inputLines(implicit mode: Mode): List[String] = InputGetter.get(day, year, mode).toList

  def inputInts(implicit mode: Mode): Seq[Int] = inputLines(mode).map(_.toInt)

  def inputLongs(implicit mode: Mode): Seq[Long] = inputLines(mode).map(_.toLong)

  def inputBlob(implicit mode: Mode): String = inputLines(mode).mkString("\n")
}
