package aoc2022

import util.AocTools
import util.InputGetter.{Example, Live, Mode}

object Day11 extends AocTools(11, 2022) {
//  implicit private val mode: Mode = Example
  implicit private val mode: Mode = Live

  case class Monkey(no: Int, items: List[Long], operation: String, test: Int, ifTrue: Int, ifFalse: Int)

  def parseMonkey(input: List[String]): Monkey = {
    val no = input.head match { case s"Monkey $no:" => no.toInt }
    val startingItems = input(1).trim match { case s"Starting items: $items" => items.split(", ").map(_.toLong) }
    val operation = input(2).drop(19)
    val test = input(3).trim match { case s"Test: divisible by $i" => i.toInt }
    val ifTrue = input(4).trim match { case s"If true: throw to monkey $i" => i.toInt }
    val ifFalse = input(5).trim match { case s"If false: throw to monkey $i" => i.toInt }
    Monkey(no, startingItems.toList, operation, test, ifTrue, ifFalse)
  }

  val monkeys = inputLines.grouped(7).map(parseMonkey).toList

  case class GameState(monkeys: List[Monkey], inspectionHistory: Map[Int, Long])

  def playRound(gameState: GameState, div: Boolean): GameState = {
    monkeys.foldLeft(gameState) { (gameState, monkey) =>
      step(monkey.no, gameState, div)
    }
  }

  def step(monkeyNo: Int, gameState: GameState, div: Boolean): GameState = {
    val currMonkey = gameState.monkeys(monkeyNo)
    
    val newGameState = currMonkey.items.foldLeft(gameState) { (gameState, item) =>
      val newWorryLevel = {
        val wl = evaluate(item, currMonkey.operation)
        if (div) wl / 3 else wl
      }
      val testItem = newWorryLevel % currMonkey.test == 0
      val throwTo = if (testItem) currMonkey.ifTrue else currMonkey.ifFalse
      val valueToThrow = newWorryLevel % monkeys.map(_.test).product
      val updatedReceivingMonkey = gameState.monkeys(throwTo).copy(items = gameState.monkeys(throwTo).items :+ valueToThrow)
      val newMonkeys = gameState.monkeys.updated(throwTo, updatedReceivingMonkey)
      gameState.copy(monkeys = newMonkeys)
    }
    
    GameState(
      monkeys = newGameState.monkeys.updated(currMonkey.no, currMonkey.copy(items = List.empty)),
      inspectionHistory = gameState.inspectionHistory.updatedWith(currMonkey.no)(_.map(_ + currMonkey.items.size))
    )
  }

  def evaluate(item: Long, operation: String): Long = {
    operation match {
      case s"old + old" => item + item
      case s"old * old" => item * item
      case s"old * $x"  => item * x.toInt
      case s"old + $x"  => item + x.toInt
    }
  }

  def playRounds(i: Int, div: Boolean, debug: Boolean = false): GameState =
    (1 to i).foldLeft(GameState(monkeys, monkeys.map(_.no).map(no => no -> 0L).toMap))((gameState, i) => {
      val round = playRound(gameState, div)
      if (debug) printRound(round, i)
      round
    })

  def printRound(gameState: GameState, round: Int): Unit = {
    println(s"== After round $round ==")
    gameState.inspectionHistory.foreach {
      case (k, v) =>
        println(s"Monkey $k inspected items $v times")
    }
    println(gameState.monkeys.flatMap(_.items).max)
    println("")
  }

  def monkeyBusiness(inspectionHistory: Map[Int, Long]): Long = inspectionHistory.values.toList.sorted.reverse.take(2).product

  def solve1: Long = monkeyBusiness(playRounds(20, div = true).inspectionHistory)
  def solve2: Long = monkeyBusiness(playRounds(10000, div = false).inspectionHistory)

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
