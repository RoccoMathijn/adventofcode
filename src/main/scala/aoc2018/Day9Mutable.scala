package aoc2018

import scala.collection.mutable

object Day9Mutable extends App {
  val startTime = System.currentTimeMillis()

  class Circle {
    var prev: Circle = this
    var next: Circle = this
    var current: Int = 0

    def insert(marble: Int): Circle = {
      val newCircle = new Circle
      newCircle.current = marble
      newCircle.prev = next
      newCircle.next = next.next
      next.next.prev = newCircle
      next.next = newCircle
      newCircle
    }

    def remove: (Circle, Int) = {
      var position = this
      for (_ <- 1 to 7) {
        position = position.prev
      }
      position.next.prev = prev
      position.prev.next = position.next
      (position.next, position.current)
    }

    def prettyPrint(): Unit = {
      var position = this
      if (position.current == 0) {
        print(s"${position.current} ")
        while (position.next.current != 0) {
          position = position.next
          print(s"${position.current} ")
        }
      } else {
        while (position.current != 0) {
          position = position.next
        }
        position.prettyPrint()
      }
    }
  }

  val players = 405
  val lastMarble =  7170000
//  val players = 9
//  val lastMarble = 25

  val playersRepeating = Stream.continually(1 to players).flatten
  val marbles = 1 to lastMarble
  val playerMarbles: Stream[(Int, Int)] = playersRepeating.zip(marbles)

  val scoreBoard = mutable.Map[Int, Long]().withDefaultValue(0)
  var circle = new Circle

  for ((player, marble) <- playerMarbles) {
    if (marble % 23 == 0) {
      val (newCircle, removedMarble) = circle.remove
      circle = newCircle
      scoreBoard(player) = scoreBoard(player) + removedMarble + marble
    } else {
      circle = circle.insert(marble)
    }
  }

  println(scoreBoard.maxBy(_._2))

  val endTime = System.currentTimeMillis()
  println(s"Runtime: ${(endTime - startTime)/1000} seconds")
}
