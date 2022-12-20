package aoc2022

import org.scalatest.funsuite.AnyFunSuite

class Day20Test extends AnyFunSuite {
  test("WrapAround") {
    assert(Day20Helper.wrapAround(0, 5) == 0)
    assert(Day20Helper.wrapAround(1, 5) == 1)
    assert(Day20Helper.wrapAround(2, 5) == 2)
    assert(Day20Helper.wrapAround(3, 5) == 3)
    assert(Day20Helper.wrapAround(4, 5) == 0)
    assert(Day20Helper.wrapAround(5, 5) == 1)
    assert(Day20Helper.wrapAround(6, 5) == 2)
    assert(Day20Helper.wrapAround(7, 5) == 3)
    assert(Day20Helper.wrapAround(8, 5) == 0)
    assert(Day20Helper.wrapAround(9, 5) == 1)
    assert(Day20Helper.wrapAround(10, 5) == 2)
    assert(Day20Helper.wrapAround(11, 5) == 3)
    assert(Day20Helper.wrapAround(3246356612L, 5) == 0)
    assert(Day20Helper.wrapAround(3246356613L, 5) == 1)
    assert(Day20Helper.wrapAround(3246356614L, 5) == 2)
    assert(Day20Helper.wrapAround(3246356615L, 5) == 3)
    assert(Day20Helper.wrapAround(3246356616L, 5) == 0)
    assert(Day20Helper.wrapAround(-1L, 5) == 3)
    assert(Day20Helper.wrapAround(-2L, 5) == 2)
    assert(Day20Helper.wrapAround(-3L, 5) == 1)
    assert(Day20Helper.wrapAround(-4L, 5) == 0)
    assert(Day20Helper.wrapAround(-5L, 5) == 3)
    assert(Day20Helper.wrapAround(-6L, 5) == 2)
    assert(Day20Helper.wrapAround(-7L, 5) == 1)
    assert(Day20Helper.wrapAround(-8L, 5) == 0)
  }
  test("Move element at index N positions") {
    val list = List(1, 2, 3, 4, 5).map(_.toLong).zipWithIndex

    assert(Day20Helper.moveIndexByPositions(0, 0, list).map(_._1) == List(1, 2, 3, 4, 5))
    assert(Day20Helper.moveIndexByPositions(0, 1, list).map(_._1) == List(2, 1, 3, 4, 5))
    assert(Day20Helper.moveIndexByPositions(0, 4, list).map(_._1) == List(1, 2, 3, 4, 5))
    assert(Day20Helper.moveIndexByPositions(0, 4 * 2 ^ 100, list).map(_._1) == List(1, 2, 3, 4, 5))
    assert(Day20Helper.moveIndexByPositions(0, -4 * 2 ^ 100, list).map(_._1) == List(1, 2, 3, 4, 5))
    assert(Day20Helper.moveIndexByPositions(0, 5, list).map(_._1) == List(2, 1, 3, 4, 5))
    assert(Day20Helper.moveIndexByPositions(0, 5, list).map(_._1) == List(2, 1, 3, 4, 5))
    assert(Day20Helper.moveIndexByPositions(0, 8, list).map(_._1) == List(1, 2, 3, 4, 5))
    assert(Day20Helper.moveIndexByPositions(4, 1, list).map(_._1) == List(1, 5, 2, 3, 4))
    assert(Day20Helper.moveIndexByPositions(0, -1, list).map(_._1) == List(2, 3, 4, 1, 5))
    assert(Day20Helper.moveIndexByPositions(0, -4, list).map(_._1) == List(1, 2, 3, 4, 5))
    assert(Day20Helper.moveIndexByPositions(0, -5, list).map(_._1) == List(2, 3, 4, 1, 5))
    assert(Day20Helper.moveIndexByPositions(0, -4 * 2 ^ 100 - 1, list).map(_._1) == List(2, 3, 4, 1, 5))
    assert(Day20Helper.moveIndexByPositions(0, -9, list).map(_._1) == List(2, 3, 4, 1, 5))
    assert(Day20Helper.moveIndexByPositions(0, -13, list).map(_._1) == List(2, 3, 4, 1, 5))
    assert(Day20Helper.moveIndexByPositions(0, -8, list).map(_._1) == List(1, 2, 3, 4, 5))
    assert(Day20Helper.moveIndexByPositions(0, -12, list).map(_._1) == List(1, 2, 3, 4, 5))

    val list2 = List(-2, 1, 2, -3, 0, 3, 4).map(_.toLong).zipWithIndex
    assert(Day20Helper.moveIndexByPositions(6, 4, list2).map(_._1) == List(-2, 1, 2, -3, 4, 0, 3))
  }
  
  test("Nth number after zero") {
    val list = List(1, 2, 0, 4, 5).map(_.toLong)
    assert(Day20Helper.nthNumberAfterZero(1, list) == 4)
    assert(Day20Helper.nthNumberAfterZero(2, list) == 5)
    assert(Day20Helper.nthNumberAfterZero(3, list) == 1)
    assert(Day20Helper.nthNumberAfterZero(4, list) == 2)
    assert(Day20Helper.nthNumberAfterZero(5, list) == 0)
    assert(Day20Helper.nthNumberAfterZero(6, list) == 4)
    assert(Day20Helper.nthNumberAfterZero(7, list) == 5)
    assert(Day20Helper.nthNumberAfterZero(8, list) == 1)
  }

  test("Mix") {
    val res = Day20Helper.mix(List(811589153L, 1623178306L, -2434767459L, 2434767459L, -1623178306L, 0L, 3246356612L).zipWithIndex)
    assert(res == List((0, 5), (-2434767459L, 2), (3246356612L, 6), (-1623178306L, 4), (2434767459L, 3), (1623178306L, 1), (811589153L, 0)))
  }
}
