package aoc2022

import util.AocTools
import util.InputGetter.{Example, Live, Mode}

import scala.annotation.tailrec

object Day13 extends AocTools(13, 2022) {
//  implicit private val mode: Mode = Example
  implicit private val mode: Mode = Live

  val input: List[(PacketData, PacketData)] = inputLines.grouped(3).map(_.take(2)).toList.map(_.map(parse)).map(group => group.head -> group.last)
  assert(inputLines.filter(_.nonEmpty).forall(line => toString(parse(line)) == line))

  sealed trait PacketData extends Ordered[PacketData] {
    override def compare(that: PacketData): Int =
      comparePackets(this, that) match {
        case Some(true)  => -1
        case Some(false) => 1
        case _           => 0
      }
  }
  case class PacketList(value: List[PacketData]) extends PacketData
  case class Integer(value: Int) extends PacketData

  def parse(input: String): PacketData = {
    input match {
      case input if input.startsWith("[") =>
        val items = splitList(input.tail.init)
        PacketList(items.map(parse))
      case input => Integer(input.toInt)
    }
  }

  def splitList(string: String): List[String] = {
    if (string.isEmpty) List.empty
    else if (string.head.isDigit) string.takeWhile(_ != ',') :: splitList(string.dropWhile(_ != ',').drop(1))
    else {
      val (first, rest) = takeUntilClosingChar(string.tail)
      first :: splitList(rest)
    }
  }

  @tailrec
  def takeUntilClosingChar(string: String, acc: String = "[", counter: Int = 1): (String, String) = {
    if (counter == 0) acc -> string.drop(1)
    else {
      string.head match {
        case '['       => takeUntilClosingChar(string.tail, acc :+ '[', counter + 1)
        case ']'       => takeUntilClosingChar(string.tail, acc :+ ']', counter - 1)
        case otherwise => takeUntilClosingChar(string.tail, acc :+ otherwise, counter)
      }
    }
  }

  def toString(packetData: PacketData): String = {
    packetData match {
      case PacketList(items) => s"[${items.map(toString).mkString(",")}]"
      case Integer(value)    => value.toString
    }
  }

  def comparePackets(left: PacketData, right: PacketData): Option[Boolean] = {
    (left, right) match {
      case (Integer(lVal), Integer(rVal)) if lVal < rVal => Some(true)
      case (Integer(lVal), Integer(rVal)) if lVal > rVal => Some(false)
      case (Integer(_), Integer(_))                      => None

      case (PacketList(Nil), PacketList(Nil))         => None
      case (PacketList(Nil), PacketList(_))           => Some(true)
      case (PacketList(_), PacketList(Nil))           => Some(false)
      case (PacketList(l :: lx), PacketList(r :: rx)) => comparePackets(l, r).orElse(comparePackets(PacketList(lx), PacketList(rx)))

      case (l, Integer(r)) => comparePackets(l, PacketList(List(Integer(r))))
      case (Integer(l), r) => comparePackets(PacketList(List(Integer(l))), r)
    }
  }

  def solve1: Int = input.map(t => comparePackets(t._1, t._2)).zipWithIndex.filter(t => t._1.contains(true)).map(_._2).map(_ + 1).sum

  def solve2: Int = {
    val dividerPackets = List(parse("[[2]]"), parse("[[6]]"))
    (input.flatMap { case (left, right) => List(left, right) } ++ dividerPackets)
      .sorted
      .zipWithIndex
      .filter { case (packet, _) => dividerPackets.contains(packet) }
      .map { case (_, i) => i + 1 }
      .product
  }

  def main(args: Array[String]): Unit = {
    val start = System.currentTimeMillis()
    println(s"AOC 2022 - Day $day")

    val part1 = solve1
    val mid = System.currentTimeMillis()
    println(s"Answer part 1: $part1 [${mid - start}ms]")
    val part2 = solve2
    val end = System.currentTimeMillis()
    println(s"Answer part 2: $part2 [${end - mid}ms]")
  }
}
