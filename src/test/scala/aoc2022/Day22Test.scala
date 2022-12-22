package aoc2022

import aoc2022.Day22Helper._
import aoc2022.Prelude._
import org.scalatest.funsuite.AnyFunSuite
class Day22Test extends AnyFunSuite {
  test("travel right") {
    val state = State(Point(0, 0), Right)
    val board = List("....".toList)
    assert(Day22Helper.travel(state, Forward(0), board).position == Point(0, 0))
    assert(Day22Helper.travel(state, Forward(1), board).position == Point(1, 0))
    assert(Day22Helper.travel(state, Forward(3), board).position == Point(3, 0))
    assert(Day22Helper.travel(state, Forward(4), board).position == Point(0, 0))
  }
  test("travel left") {
    val state = State(Point(3, 0), Left)
    val board = List("....".toList)
    assert(Day22Helper.travel(state, Forward(1), board).position == Point(2, 0))
    assert(Day22Helper.travel(state, Forward(3), board).position == Point(0, 0))
    assert(Day22Helper.travel(state, Forward(4), board).position == Point(3, 0))
  }
  test("travel down") {
    val state = State(Point(0, 0), Down)
    val board = List(List('.'), List('.'), List('.'), List('.'))
    assert(Day22Helper.travel(state, Forward(1), board).position == Point(0, 1))
    assert(Day22Helper.travel(state, Forward(3), board).position == Point(0, 3))
    assert(Day22Helper.travel(state, Forward(4), board).position == Point(0, 0))
  }
  test("travel up") {
    val state = State(Point(0, 3), Up)
    val board = List(List('.'), List('.'), List('.'), List('.'))
    assert(Day22Helper.travel(state, Forward(1), board).position == Point(0, 2))
    assert(Day22Helper.travel(state, Forward(3), board).position == Point(0, 0))
    assert(Day22Helper.travel(state, Forward(4), board).position == Point(0, 3))
  }

  test("travel right with padding") {
    val state = State(Point(1, 0), Right)
    val board = List(" .... ".toList)
    assert(Day22Helper.travel(state, Forward(1), board).position == Point(2, 0))
    assert(Day22Helper.travel(state, Forward(3), board).position == Point(4, 0))
    assert(Day22Helper.travel(state, Forward(4), board).position == Point(1, 0))
  }
  test("travel left with padding") {
    val state = State(Point(4, 0), Left)
    val board = List(" .... ".toList)
    assert(Day22Helper.travel(state, Forward(1), board).position == Point(3, 0))
    assert(Day22Helper.travel(state, Forward(3), board).position == Point(1, 0))
    assert(Day22Helper.travel(state, Forward(4), board).position == Point(4, 0))
  }
  test("travel down with padding") {
    val state = State(Point(0, 1), Down)
    val board = List(List(' '), List('.'), List('.'), List('.'), List('.'), List(' '))
    assert(Day22Helper.travel(state, Forward(1), board).position == Point(0, 2))
    assert(Day22Helper.travel(state, Forward(3), board).position == Point(0, 4))
    assert(Day22Helper.travel(state, Forward(4), board).position == Point(0, 1))
  }
  test("travel up with padding") {
    val state = State(Point(0, 4), Up)
    val board = List(List(' '), List('.'), List('.'), List('.'), List('.'), List(' '))
    assert(Day22Helper.travel(state, Forward(1), board).position == Point(0, 3))
    assert(Day22Helper.travel(state, Forward(3), board).position == Point(0, 1))
    assert(Day22Helper.travel(state, Forward(4), board).position == Point(0, 4))
  }
  test("walls") {
    val state = State(Point(0, 0), Right)
    val board = List("...#".toList)
    assert(Day22Helper.travel(state, Forward(3), board).position == Point(2, 0))
    assert(Day22Helper.travel(state, Forward(4), board).position == Point(2, 0))

  }
  test("walls2") {
    val state = State(Point(1, 0), Left)
    val board = List("...#".toList)
    assert(Day22Helper.travel(state, Forward(1), board).position == Point(0, 0))
    assert(Day22Helper.travel(state, Forward(4), board).position == Point(0, 0))
  }
  test("walls3") {
    val state = State(Point(1, 0), Left)
    val board = List(" ...# ".toList)
    assert(Day22Helper.travel(state, Forward(1), board).position == Point(1, 0))
    assert(Day22Helper.travel(state, Forward(4), board).position == Point(1, 0))
  }
  test("wrap around cube") {
    // face 1
    assert(wrapAroundCube(Point(50, -1), Up) == Point(0, 150) -> Right)
    assert(wrapAroundCube(Point(99, -1), Up) == Point(0, 199) -> Right)
    
    assert(wrapAroundCube(Point(49, 0), Left) == Point(0, 149) -> Right)
    assert(wrapAroundCube(Point(49, 49), Left) == Point(0, 100) -> Right)

    // face 2
    assert(wrapAroundCube(Point(100, -1), Up) == Point(0, 199) -> Up)
    assert(wrapAroundCube(Point(149, -1), Up) == Point(49, 199) -> Up)
    
    assert(wrapAroundCube(Point(100, 50), Down) == Point(99, 50) -> Left)
    assert(wrapAroundCube(Point(149, 50), Down) == Point(99, 99) -> Left)

    assert(wrapAroundCube(Point(150, 0), Right) == Point(99, 149) -> Left)
    assert(wrapAroundCube(Point(150, 49), Right) == Point(99, 100) -> Left)

    // face 3
    assert(wrapAroundCube(Point(49, 50), Left) == (Point(0, 100) -> Down))
    assert(wrapAroundCube(Point(49, 99), Left) == (Point(49, 100) -> Down))

    assert(wrapAroundCube(Point(100, 50), Right) == (Point(100, 49) -> Up))
    assert(wrapAroundCube(Point(100, 99), Right) == (Point(149, 49) -> Up))

    // face 4
    assert(wrapAroundCube(Point(50, 150), Down) == (Point(49, 150) -> Left))
    assert(wrapAroundCube(Point(99, 150), Down) == (Point(49, 199) -> Left))
    
    assert(wrapAroundCube(Point(100, 149), Right) == (Point(149, 0) -> Left))
    assert(wrapAroundCube(Point(100, 100), Right) == (Point(149, 49) -> Left))

    // face 5
    assert(wrapAroundCube(Point(-1, 100), Left) == (Point(50, 49) -> Right))
    assert(wrapAroundCube(Point(-1, 149), Left) == (Point(50, 0) -> Right))

    assert(wrapAroundCube(Point(0, 99), Up) == (Point(50, 50) -> Right))
    assert(wrapAroundCube(Point(49, 99), Up) == (Point(50, 99) -> Right))
    
    // face 6
    assert(wrapAroundCube(Point(0, 200), Down) == (Point(100, 0) -> Down))
    assert(wrapAroundCube(Point(49, 200), Down) == (Point(149, 0) -> Down))
    
    assert(wrapAroundCube(Point(50, 150), Right) == (Point(50, 149) -> Up))
    assert(wrapAroundCube(Point(50, 199), Right) == (Point(99, 149) -> Up))
    
    assert(wrapAroundCube(Point(-1, 150), Left) == (Point(50, 0) -> Down))
    assert(wrapAroundCube(Point(-1, 199), Left) == (Point(99, 0) -> Down))

  }
}
