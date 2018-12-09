package aoc2017

import org.scalatest.WordSpec

import scala.io.Source

class Day01Test extends WordSpec {
  val sample1: List[Int] = Source
    .fromResource("aoc2017/input-day1-sample1.txt")
    .map(c => Integer.parseInt(c.toString))
    .toList

  val sample2: List[Int] = Source
    .fromResource("aoc2017/input-day1-sample2.txt")
    .map(c => Integer.parseInt(c.toString))
    .toList

  val day1: List[Int] = Source
    .fromResource("aoc2017/input-day1.txt")
    .map(c => Integer.parseInt(c.toString))
    .toList

  "Part1" should {
    "sample1" in {
      assert(Day01.solve(sample1 :+ sample1.head) == 3)
    }

    "sample2" in {
      assert(Day01.solve(sample2 :+ sample2.head) == 9)
    }

    "day1" in {
      println(Day01.solve(day1 :+ day1.head))
    }
  }

  val sample3: List[Int] = Source
    .fromResource("aoc2017/input-day1-sample3.txt")
    .map(c => Integer.parseInt(c.toString))
    .toList

  "Part2" should {
    "sample1" in {
      assert(Day01.solve2(sample3) == 6)
    }

    "day1part2" in {
      println(Day01.solve2(day1))
    }
  }

}
