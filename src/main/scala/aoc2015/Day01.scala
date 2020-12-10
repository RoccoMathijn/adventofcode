package aoc2015

import util.AocTools
import util.InputGetter.{Example, Live}

object Day01 extends AocTools(1, 2015) {
//  implicit val mode = Example
  implicit val mode = Live

  def main(args: Array[String]): Unit = {
    val res = inputLines.head.zipWithIndex.foldLeft(0) { (a, b) =>
      val floor = b._1 match {
        case '(' => a + 1
        case ')' => a - 1
      }
      if (floor == - 1) println(b._2 + 1)
      floor
    }

    println(res)
  }
}
