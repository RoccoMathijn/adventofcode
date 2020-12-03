package aoc2019

import scala.io.Source

object Day02 extends App {
  val input: List[Int] = Source
    .fromResource("aoc2019/input-day2.txt")
    .getLines()
    .mkString
    .split(',')
    .map(_.toInt)
    .toList

  def computer(input: List[Int], startPos: Int): List[Int] = {
    input(startPos) match {
      case 1 =>
        computer(
          input.updated(
            input(startPos + 3),
            input(input(startPos + 1)) + input(input(startPos + 2))
          ),
          startPos + 4
        )
      case 2 =>
        computer(
          input.updated(
            input(startPos + 3),
            input(input(startPos + 1)) * input(input(startPos + 2))
          ),
          startPos + 4
        )
      case 99 => input
      case _  => throw new Error("Something went wrong!")
    }
  }

  def run(input: List[Int], noun: Int, verb: Int) =
    computer(input.updated(1, noun).updated(2, verb), 0)(0)

  println(run(input, 12, 2))

  val res2 = for {
    noun <- 0 to 99
    verb <- 0 to 99
    output = run(input, noun, verb)
    if output == 19690720
  } yield 100 * noun + verb

  println(res2)
}
