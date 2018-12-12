package aoc2018

import scala.io.Source

object Day12 extends App {
  val startTime = System.currentTimeMillis()

  val input = Source
    .fromResource("aoc2018/input-day12.txt")
    .getLines
    .toList

  val initialState = parseInitialState(input.head)
  val rules: Map[List[Char], Char] = input.tail.drop(1).map(parseRule).toMap

  def parseInitialState(line: String): Set[Int] = line.drop(15).toList.zipWithIndex.filter(_._1 == '#').map(_._2).toSet

  def parseRule(line: String): (List[Char], Char) = {
    val rule = line.take(5)
    val state = line.last
    (rule.toList, state)
  }

  def evolve(state: Set[Int]): Set[Int] = {
    val considerOtherIndexes: Set[Int] = state.flatMap(i => List(i - 2, i - 1, i, i + 1, i + 2))
    val withNeighbouringIndexes: Set[(Int, List[Char])] = considerOtherIndexes.map(i => i ->
      List(hasPlant(i - 2, state), hasPlant(i - 1, state), hasPlant(i, state), hasPlant(i + 1, state), hasPlant(i + 2, state)))
    withNeighbouringIndexes.map(x => x._1 -> rules.getOrElse(x._2, '.')).filter(_._2 == '#').map(_._1)
  }

  def hasPlant(i: Int, state: Set[Int]) = if (state.contains(i)) '#' else '.'

  val result: Int = (1 to 20).foldLeft(initialState)((state, _) => evolve(state)).sum

  println(result)

  def run(state: Set[Int], diff: Int, generation: Long): Long = {
    val newState = evolve(state)
    val newDiff = newState.sum - state.sum
    if (newDiff == diff) {
      newState.sum + (50000000000l - generation) * newDiff
    }
    else run(newState, newDiff, generation + 1)
  }

  println(run(initialState, 0, 1))

  val endTime = System.currentTimeMillis()
  println(s"Runtime: ${(endTime - startTime) / 1000} seconds")
}
