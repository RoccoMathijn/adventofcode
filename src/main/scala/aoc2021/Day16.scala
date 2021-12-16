package aoc2021

import util.AocTools
import util.InputGetter.{Live, Mode}

import java.math.BigInteger

object Day16 extends AocTools(16, 2021) {
//  implicit private val mode: Mode = Example
  implicit private val mode: Mode = Live

  val hexMap: Map[Char, String] =
    Map(
      '0' -> "0000",
      '1' -> "0001",
      '2' -> "0010",
      '3' -> "0011",
      '4' -> "0100",
      '5' -> "0101",
      '6' -> "0110",
      '7' -> "0111",
      '8' -> "1000",
      '9' -> "1001",
      'A' -> "1010",
      'B' -> "1011",
      'C' -> "1100",
      'D' -> "1101",
      'E' -> "1110",
      'F' -> "1111"
    )

  val hexString: String = inputLines.head
  val bitString: String = hexString.toList.map(hexMap).mkString

  sealed trait Packet {
    val version: Int
    val typeId: Int
  }
  case class Literal(version: Int, typeId: Int, value: Long) extends Packet
  case class Operator(version: Int, typeId: Int, value: List[Packet]) extends Packet

  def parseBitString(string: String): (Packet, String) = {
    val version = Integer.parseInt(string.take(3), 2)
    val typeId = Integer.parseInt(string.slice(3, 6), 2)

    val remainder = string.drop(6)
    val result = if (typeId == 4) {
      val (binaryNumber, rem) = parseLiteral(remainder)
      Literal(version, typeId, binaryNumber) -> rem
    } else {
      val (packets, rem) = parseOperator(remainder)
      Operator(version, typeId, packets) -> rem
    }
    println(result._1)
    result
  }

  def parseLiteral(string: String, acc: List[String] = List.empty): (Long, String) = {
    if (string.head == '1')
      parseLiteral(string.drop(5), acc :+ string.slice(1, 5))
    else {
      val byteList = (acc :+ string.slice(1, 5)).mkString
      val number: Long = new BigInteger(byteList, 2).longValue()

      number -> string.drop(5)
    }
  }

  def parseOperator(string: String): (List[Packet], String) = {
    val lengthTypeId = string.head
    println(s"Length type id: $lengthTypeId")
    if (lengthTypeId == '0') {
      val length = Integer.parseInt(string.slice(1, 16), 2)
      val rem = string.drop(16)
      println(s"Length: $length")
      def loop(remainder: String, length: Int, acc: List[Packet] = List.empty): (List[Packet], String) = {
        if (length <= 0) acc -> remainder
        else {
          val (packet, rem) = parseBitString(remainder)
          loop(rem, length - (remainder.length - rem.length), acc :+ packet)
        }
      }
      loop(rem, length)
    } else {
      val nSubPackets = Integer.parseInt(string.slice(1, 12), 2)
      println(s"Subpackets: $nSubPackets")

      (1 to nSubPackets).foldLeft(List.empty[Packet] -> string.drop(12)) { (acc, _) =>
        val (packet, rem) = parseBitString(acc._2)
        (acc._1 :+ packet) -> rem
      }
    }
  }

  def sumVersions(packet: Packet): Int = {
    packet match {
      case Literal(version, _, _)      => version
      case Operator(version, _, value) => version + value.map(sumVersions).sum
    }
  }

  def expression(packet: Packet): Long = {
    packet match {
      case Literal(_, _, value)  => value
      case Operator(_, 0, value) => value.map(expression).sum
      case Operator(_, 1, value) => value.map(expression).product
      case Operator(_, 2, value) => value.map(expression).min
      case Operator(_, 3, value) => value.map(expression).max
      case Operator(_, 5, value) =>
        val List(x1, x2) = value.map(expression)
        if (x1 > x2) 1 else 0
      case Operator(_, 6, value) =>
        val List(x1, x2) = value.map(expression)
        if (x1 < x2) 1 else 0
      case Operator(_, 7, value) =>
        val List(x1, x2) = value.map(expression)
        if (x1 == x2) 1 else 0
    }
  }
  def solve1: Int = sumVersions(parseBitString(bitString)._1)
  def solve2: Long = expression(parseBitString(bitString)._1)

  def main(args: Array[String]): Unit = {
    val start = System.currentTimeMillis()
    println(s"AOC $year - Day $day")

    val part1 = solve1

    val mid = System.currentTimeMillis()
    println(s"Answer part 1: $part1 [${mid - start}ms]")

    val part2 = solve2
    val end = System.currentTimeMillis()
    println(s"Answer part 2: $part2 [${end - mid}ms]")
  }
}
