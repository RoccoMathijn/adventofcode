package aoc2019

object Day04 extends App {
  var count = 0
  (367479 to 893698).map { i =>
    if (twoAdjacentSameSame(i.toString) && noDecrease(i.toString))
      count += 1
  }

  var count2 = 0
  (367479 to 893698).map { i =>
    if (twoAdjacentSameSameAndNotPartOfLargerGroup(i.toString) && noDecrease(
          i.toString
        ))
      count2 += 1
  }

  def twoAdjacentSameSame(i: String) =
    i.sliding(2).exists { ss =>
      ss.head == ss.last
    }

  def noDecrease(i: String) =
    i.sliding(2).forall { ss =>
      ss.head <= ss.last
    }

  def twoAdjacentSameSameAndNotPartOfLargerGroup(input: String): Boolean = {
    input
      .foldLeft(List.empty[String]) { (acc, char) =>
        if (acc.headOption.flatMap(_.headOption).contains(char))
          (acc.head :+ char) :: acc.tail
        else char.toString :: acc
      }
      .exists(_.length == 2)
  }

  println(count)
  println(count2)
}
