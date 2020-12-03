package aoc2020

import scala.collection.immutable
import scala.io.Source

object Day01 extends App {
  val input: immutable.Seq[Int] = Source
    .fromResource("aoc2020/input-day1.txt")
    .getLines()
    .map(_.toInt)
    .toList

  input.foreach { x =>
    input.foreach { y =>
         if (x+y == 2020) {
           println(x*y)
         }
    }
  }

  input.foreach { x =>
    input.foreach { y =>
      input.foreach { z =>
        if (x + y + z == 2020) {
          println(x * y * z)
        }
      }
    }
  }
}
