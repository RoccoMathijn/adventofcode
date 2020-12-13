package aoc2020

import util.AocTools
import util.InputGetter.{Live, Mode}

import scala.collection.immutable.LazyList
import scala.util.{Success, Try}

object Day13 extends AocTools(13, 2020) {
//  implicit private val mode: Mode = Example
  implicit private val mode: Mode = Live

  val myTs: Int = inputLines.head.toInt
  private val busIds: Seq[(Int, Int)] = inputLines.last.split(',').zipWithIndex.filterNot(_._1 == "x").map(x => x._1.toInt -> x._2).toList

  def firstMultiple(from: Long, step: Long): Long = {
    if (from % step == 0) from
    else firstMultiple(from + 1, step)
  }

  def buildLazyList(from: Long, step: Long): LazyList[Long] = {
    LazyList.cons(from, buildLazyList(from + step, step))
  }

  type Schedule = Seq[((Int, Int), LazyList[Long])]
  val schedule: Schedule = busIds.map(id => id -> buildLazyList(firstMultiple(0, id._1.toLong), id._1.toLong))

  def findFirst(schedule: Schedule): Long = {
    val (busId, earliestTs) = schedule.map(x => x._1 -> x._2.find(_ >= myTs).get).minBy(_._2)
    busId._1 * (earliestTs - myTs)
  }

  def run2(): Option[Long] = {
    chineseRemainder(busIds.map(_._1.toLong).toList, busIds.map(b => b._1.toLong - b._2.toLong).toList)
  }

  def chineseRemainder(n: List[Long], a: List[Long]): Option[Long] = {
    require(n.size == a.size)
    val prod = n.product

    def iter(n: List[Long], a: List[Long], sm: Long): Long = {
      def mulInv(a: Long, b: Long): Long = {
        def loop(a: Long, b: Long, x0: Long, x1: Long): Long = {
          if (a > 1) loop(b, a % b, x1 - (a / b) * x0, x0) else x1
        }

        if (b == 1) 1
        else {
          val x1 = loop(a, b, 0, 1)
          if (x1 < 0) x1 + b else x1
        }
      }

      if (n.nonEmpty) {
        val p = prod / n.head

        iter(n.tail, a.tail, sm + a.head * mulInv(p, n.head) * p)
      } else sm
    }

    Try {
      iter(n, a, 0) % prod
    } match {
      case Success(v) => Some(v)
      case _          => None
    }
  }

  def main(args: Array[String]): Unit = {
    println(s"AOC 2020 - Day $day")

    val part1 = findFirst(schedule)
    println(s"Answer part 1: $part1")

    val part2 = run2()
    println(s"Answer part 2: $part2")
  }
}
