package aoc2020

import util.AocTools
import util.InputGetter.{Example, Live, Mode}

import scala.annotation.tailrec

object Day22 extends AocTools(22, 2020) {
//  implicit private val mode: Mode = Example
  implicit private val mode: Mode = Live
  private val input: Seq[(String, Array[Int])] = inputBlob
    .split("\n\n")
    .map { player =>
      val lines = player.split("\n")
      val name = lines.head
      val cards = lines.tail.map(_.toInt)
      name -> cards
    }
    .toList

  def calculateScore(player: List[Int]): Int = player.reverse.zipWithIndex.map { case (card, index) => card * (index + 1) }.sum
  def playCombat(player1: List[Int], player2: List[Int]): Int = {
    (player1, player2) match {
      case (player1, Nil) =>
        println("Player1 wins")
        calculateScore(player1)
      case (Nil, player2) =>
        println("Player2 wins")
        calculateScore(player2)
      case (x :: xs, y :: ys) =>
        if (x > y)
          playCombat(xs :+ x :+ y, ys)
        else
          playCombat(xs, ys :+ y :+ x)
    }
  }

  sealed trait Winner
  case class P1(score: Int) extends Winner
  case class P2(score: Int) extends Winner

  def playRecursiveCombat(player1: List[Int], player2: List[Int], h1: Set[List[Int]] = Set.empty, h2: Set[List[Int]] = Set.empty): Winner = {
    if (h1(player1) && h2(player2)) {
      println("P1 wins by same configuration")
      P1(calculateScore(player1))
    } else {
      println(s"Player 1's deck: ${player1.mkString(",")}")
      println(s"Player 2's deck: ${player2.mkString(",")}")
      (player1, player2) match {
        case (player1, Nil) =>
          println("Player1 wins")
          P1(calculateScore(player1))
        case (Nil, player2) =>
          println("Player2 wins")
          P2(calculateScore(player2))
        case (x :: xs, y :: ys) =>
          if (xs.size >= x && ys.size >= y) {
            println("Playing a sub-game to determine the winner...")
            playRecursiveCombat(xs.take(x), ys.take(y), h1, h2) match {
              case _: P1 => playRecursiveCombat(xs :+ x :+ y, ys, h1 + player1, h2 + player2)
              case _: P2 => playRecursiveCombat(xs, ys :+ y :+ x, h1 + player1, h2 + player2)
            }
          } else if (x > y) {
            println("Player 1 wins the round!\n")
            playRecursiveCombat(xs :+ x :+ y, ys, h1 + player1, h2 + player2)
          } else {
            println("Player 2 wins the round!\n")
            playRecursiveCombat(xs, ys :+ y :+ x, h1 + player1, h2 + player2)
          }
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val start = System.currentTimeMillis()
    println(s"AOC 2020 - Day $day")

    val part1 = playCombat(input(0)._2.toList, input(1)._2.toList)
    val mid = System.currentTimeMillis()
    println(s"Answer part 1: $part1 [${mid - start}ms]")

    val part2 = playRecursiveCombat(input(0)._2.toList, input(1)._2.toList)
    val end = System.currentTimeMillis()
    println(s"Answer part 2: $part2 [${`end` - mid}ms]")
  }
}
