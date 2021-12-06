package aoc2021

import util.AocTools
import util.InputGetter.{Live, Mode, Example}

object Day6 extends AocTools(6, 2021) {
//  implicit private val mode: Mode = Example
  implicit private val mode: Mode = Live

  val input: Seq[Int] = inputLines.head.split(',').map(_.toInt).toList

  def simulate(fish: Seq[Int]): Seq[Int] = fish.foldLeft(Seq.empty[Int])((acc, fish) => if (fish == 0) 6 +: 8 +: acc else (fish - 1) +: acc)

  val cache = scala.collection.mutable.Map.empty[(Int, Int), Long]
  
  def calculate(fish: Int, days: Int): Long = {
    if (days == 0) 1
    else {
      val result: Long = cache.getOrElse(
        fish -> days, {
          simulate(List(fish)).foldLeft(0L) { (acc, fish) => acc + calculate(fish, days - 1) }
        }
      )
      cache.update(fish -> days, result)
      result
    }
  }

  def main(args: Array[String]): Unit = {
    val start = System.currentTimeMillis()
    println(s"AOC $year - Day $day")

    val part1 = input.foldLeft(0L) { (acc, fish) => acc + calculate(fish, 80) }
    val mid = System.currentTimeMillis()
    println(s"Answer part 1: $part1 [${mid - start}ms]")

    val part2 = input.foldLeft(0L) { (acc, fish) => acc + calculate(fish, 256) }
    val end = System.currentTimeMillis()
    println(s"Answer part 2: $part2 [${end - mid}ms]")
  }
}
