package aoc2018

import scala.io.Source

object Day2Part2 extends App {
  val boxIds = Source.fromResource("aoc2018/input-day2.txt").getLines().toList

  val pairs = for {
    x <- boxIds
    y <- boxIds
  } yield (x,y)


  def commonLettersAtPos(x: String, y: String) = {
    def loop(x: String, y: String, acc: String): String = {
      if (x.nonEmpty) {
        if (x.head == y.head) {
          loop(x.tail, y.tail, acc :+ x.head)
        } else {
          loop(x.tail, y.tail, acc)
        }
      } else {
        acc
      }
    }
    loop(x, y, "")
  }

  println(pairs.map(commonLettersAtPos _ tupled).filter(_.length == 25))

}
