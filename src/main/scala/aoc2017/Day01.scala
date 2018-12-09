package aoc2017

object Day01 extends App {
  def solve(input: Seq[Int]): Int = {
    input.sliding(2).foldLeft(0){ case (acc, List(fst, snd)) => if (fst == snd) acc + fst else acc }
  }

  def solve2(input: Seq[Int]): Int = {
    val length = input.length
    val half = length / 2
    input.zipWithIndex.foldLeft(0) { (acc, n) => if (n._1 == input((n._2 + half) % length)) acc + n._1 else acc }
  }
}
