package aoc2021

import util.AocTools

import scala.collection.mutable

object Day21 extends AocTools(21, 2021) {

  val player1 = Player(position = 10)
  val player2 = Player(position = 2)
  case class Player(position: Int, score: Int = 0)

  def throwDice(dice: Int, times: Int = 3, score: Int = 0): (Int, Int) = {
    if (times == 0) dice -> score
    else {
      val newDice = diceIndexed(dice % 100)
      throwDice(newDice, times - 1, score + newDice)
    }
  }

  val diceIndexed = 1 to 100
  val boardIndexed = 1 to 10

  def play(player1: Player, player2: Player, dice: Int, round: Int): Long = {
    val (diceAfterP1, p1Steps) = throwDice(dice)
    val p1NewPos = boardIndexed((player1.position - 1 + p1Steps) % 10)
    val newP1 = player1.copy(score = player1.score + p1NewPos, position = p1NewPos)
    if (newP1.score >= 1000) {
      player2.score * (round * 6 + 3)
    } else {
      val (diceAfterP2, p2Steps) = throwDice(diceAfterP1)
      val p2NewPos = boardIndexed((player2.position - 1 + p2Steps) % 10)
      val newP2 = player2.copy(score = player2.score + p2NewPos, position = p2NewPos)
      if (newP1.score >= 1000) {
        player1.score * (round * 6 + 6)
      } else {
        play(newP1, newP2, diceAfterP2, round + 1)
      }
    }
  }

  val quantumDice: Seq[Int] = for {
    one <- 1 to 3
    two <- 1 to 3
    three <- 1 to 3
  } yield one + two + three

  val cache = mutable.Map.empty[(Player, Player), (Long, Long)]

  def play2(playerA: Player, playerB: Player): (Long, Long) = {
    if (!cache.contains(playerA -> playerB)) {
      val score = quantumDice.foldLeft(0L -> 0L) { (scores, throwResult) =>
        val position = boardIndexed((playerA.position + throwResult - 1) % 10)
        val newPlayerA = playerA.copy(score = playerA.score + position, position = position)
        if (newPlayerA.score >= 21) (scores._1 + 1) -> scores._2
        else {
          val (pbWins, paWins) =  play2(playerB, newPlayerA)
          (scores._1 + paWins, scores._2 + pbWins)
        }
      }
      cache.update(playerA -> playerB, score)
    }
    cache(playerA -> playerB)
  }

  def solve1: Long = play(player1, player2, 0, round = 0)
  def solve2: Long = {
    val (p1, p2) = play2(player2, player2)
    math.max(p1, p2)
  }

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
