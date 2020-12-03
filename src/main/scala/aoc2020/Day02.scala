package aoc2020

import scala.io.Source

object Day02 extends App {
  val input: List[Line] = Source
    .fromResource("aoc2020/input-day2.txt")
    .getLines()
    .map(toLine)
    .toList


  case class Line(min: Int, max: Int, letter: Char, password: String)

  def toLine(input: String): Line = {
    val min = input.takeWhile(c => c != '-').toInt
    val max = input.dropWhile(c => c != '-').drop(1).takeWhile(c => c != ' ').toInt

    val letter = input.dropWhile(c => c != ' ').drop(1).take(1).charAt(0)
    val password = input.dropWhile(c => c != ':').drop(2)

    val line = Line(min, max, letter, password)

    line
  }

  def isValid(line: Line): Boolean = {
    val count = line.password.count(c => c == line.letter)
    count >= line.min  && count <= line.max
  }

  def isValid2(line: Line): Boolean = {
    val one = line.password.charAt(line.min - 1)
    val two = line.password.charAt(line.max - 1)
    (one == line.letter || two == line.letter) && !(one == line.letter && two == line.letter)
  }

  println(input.count(isValid))
  println(input.count(isValid2))
}
