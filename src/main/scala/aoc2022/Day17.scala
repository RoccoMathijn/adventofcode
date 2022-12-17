package aoc2022

import util.AocTools
import util.InputGetter.{Example, Live, Mode}

import scala.collection.mutable

object Day17 extends AocTools(17, 2022) {
  implicit private val mode: Mode = Example
//  implicit private val mode: Mode = Live

  val input: List[Char] = inputLines.head.toList

  case class Point(x: Int, y: Long)
  type Rock = Set[Point]
  val flatBoy = Set(Point(0, 0), Point(1, 0), Point(2, 0), Point(3, 0))
  val plus = Set(Point(1, 0), Point(0, 1), Point(1, 1), Point(2, 1), Point(1, 2))
  val el = Set(Point(0, 0), Point(1, 0), Point(2, 0), Point(2, 1), Point(2, 2))
  val tallBoy = Set(Point(0, 0), Point(0, 1), Point(0, 2), Point(0, 3))
  val squareBoy = Set(Point(0, 0), Point(1, 0), Point(0, 1), Point(1, 1))

  val rocks = List(flatBoy, plus, el, tallBoy, squareBoy)
  case class State(tower: Set[Point], jetPattern: List[Char]) {
    def adjustTower: State = {
      if (tower.nonEmpty) {
        val minY = tower.minBy(_.y).y
        this.copy(tower = tower.map(p => p.copy(y = p.y - (minY - 1))))
      } else this
    }
  }

  def fall(rock: Set[Point], state: State, counter: Long): State = {
    val blown = state.jetPattern.head match {
      case '>' => moveRight(rock)
      case '<' => moveLeft(rock)
    }
    val checked =
      if (clipped(blown, state.tower)) rock
      else blown
    val movedDown = moveDown(checked)
    if (clipped(movedDown, state.tower)) {
      val tower = state.tower ++ checked
      val reducedTower = {
        val minRow = checked.minBy(_.y).y
        val maxRow = checked.maxBy(_.y).y
        (minRow to maxRow).foldLeft(tower) { (tower, row) =>
          if (tower.count(_.y == row) == 7) {
            tower.filterNot(_.y < row)
          } else tower
        }
      }
      State(reducedTower, state.jetPattern.tail :+ state.jetPattern.head)
    } else {
      fall(movedDown, State(state.tower, state.jetPattern.tail :+ state.jetPattern.head), counter)
    }
  }

  def clipped(rock: Set[Point], tower: Set[Point]): Boolean = rock.exists(p => p.x == 0 || p.y == 0 || p.x == 8) || rock.intersect(tower).nonEmpty

  def moveDown(rock: Set[Point]): Set[Point] = rock.map(p => p.copy(y = p.y - 1))
  def moveLeft(rock: Set[Point]): Set[Point] = rock.map(p => p.copy(x = p.x - 1))
  def moveRight(rock: Set[Point]): Set[Point] = rock.map(p => p.copy(x = p.x + 1))

  def play(rocks: List[Rock], state: State, counter: Long, history: List[Long], rounds: Long): List[Long] = {
    if (counter == rounds) history
    else {
      val newState = fall(startPosition(rocks.head, state.tower), state, counter)
      play(rocks.tail :+ rocks.head, newState, counter + 1, newState.tower.maxBy(_.y).y :: history, rounds)
    }
  }

  def printTower(tower: Set[Point]): Unit = {
    val adjustedTower = State(tower, List.empty).adjustTower.tower
    val height = adjustedTower.maxBy(_.y).y
    (0L to height).reverse.foreach { y =>
      (0 to 8).foreach { x =>
        if (x == 0 || x == 8) print('|')
        else if (adjustedTower.contains(Point(x, y))) print('#')
        else if (y == 0) print('-')
        else print('.')
      }
      println("")
    }
  }

  def startPosition(rock: Set[Point], tower: Set[Point]): Set[Point] = {
    val posY = if (tower.isEmpty) 4 else tower.maxBy(_.y).y + 4
    val posX = 3
    rock.map(p => Point(p.x + posX, p.y + posY))
  }

  def solve1 = play(rocks, State(Set.empty, input), 0, List.empty, 2022).head
  
  // doensn't work for the example
  def solve2 = {
    val bigNumber = 1000000000000L
    val pattern = play(rocks, State(Set.empty, input), 0, List.empty, 2022).reverse
      .foldLeft(List(0L)) { (acc, n) =>
        acc :+ (n - acc.sum)
      }
      .drop(526)

    val patternTimes = (bigNumber - 526) / 1755
    val x = patternTimes * 2768
    val y = 805

    val leftOverSteps = ((bigNumber - 526) % 1755).toInt
    val z = pattern.take(leftOverSteps).sum
    x + y + z
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
