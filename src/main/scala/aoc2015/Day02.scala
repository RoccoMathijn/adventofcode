package aoc2015

import util.AocTools
import util.InputGetter.{Example, Live}

object Day02 extends AocTools(2, 2015) {
//  implicit val mode = Example
  implicit val mode = Live

  val pattern = "(\\d*)x(\\d*)x(\\d*)".r
  def main(args: Array[String]): Unit = {
    case class Dimensions(l: Int, h: Int, w: Int)
    val dimensions = inputLines.map {
      case pattern(l, h, w) => Dimensions(l.toInt, h.toInt, w.toInt)
    }

    val wrappingPaper = dimensions.map { d =>
      2 * d.l * d.w + 2 * d.w * d.h + 2 * d.h * d.l + math.min(math.min(d.l * d.w, d.w * d.h), d.h * d.l)
    }.sum

    val ribbon: Int = dimensions.map { d =>
      List(d.l, d.h, d.w).sorted.take(2).map(_ * 2).sum
    }.sum

    val bow = dimensions.map(d => d.l * d.h * d.w).sum

    println(wrappingPaper)
    println(bow)
    println(ribbon)
    println(ribbon + bow)
  }
}
