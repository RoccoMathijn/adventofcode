package aoc2020

import util.AocTools
import util.InputGetter.{Live, Mode}

import scala.annotation.tailrec

object Day15 extends AocTools(15, 2020) {
//  implicit private val mode: Mode = Example
  implicit private val mode: Mode = Live

  val input: Seq[Int] = inputLines.head.split(",").map(_.toInt).toList
  case class Memory(positionLast: Int, positionBefore: Option[Int])
  val startMem: Map[Int, Memory] = input.zipWithIndex.map { x => x._1 -> Memory(positionLast = x._2 + 1, positionBefore = None) }.toMap

  @tailrec
  def run(lastNumber: Int, position: Int, memory: Map[Int, Memory], until: Int): Int = {
    if (position == until + 1) lastNumber
    else {
      memory(lastNumber) match {
        case Memory(positionLast, Some(positionBefore)) =>
          val newNumber = positionLast - positionBefore
          val newMemory = memory.get(newNumber).map(m => Memory(position, Some(m.positionLast))).getOrElse(Memory(position, None))
          run(lastNumber = newNumber, position = position + 1, memory = memory.updated(newNumber, newMemory), until = until)
        case Memory(_, None) =>
          val Memory(lastPosition, _) = memory(0)
          val newMemory = Memory(positionLast = position, positionBefore = Some(lastPosition))
          run(lastNumber = 0, position = position + 1, memory = memory.updated(0, newMemory), until = until)
      }
    }
  }

  def main(args: Array[String]): Unit = {
    println(s"AOC 2020 - Day $day")
    val start = System.currentTimeMillis()


    val part1 = run(lastNumber = input.last, position = input.size + 1, memory = startMem, until = 2020)
    val mid = System.currentTimeMillis()

    println(s"Answer part 1: $part1 [${mid - start}ms]")

    val part2 = run(lastNumber = input.last, position = input.size + 1, memory = startMem, until = 30000000)
    val end = System.currentTimeMillis()
    println(s"Answer part 2: $part2 [${`end` - mid}ms]")
  }
}
