package aoc2020

abstract class AocTools(val day: Int) {

  def inputLines: List[String] = InputGetter.get(day).toList

  def inputInts: List[Int] = inputLines.map(_.toInt)

  def inputBlob = inputLines.mkString("\n")
}
