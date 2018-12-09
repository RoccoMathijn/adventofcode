package aoc2018

object Day09 extends App {
  val startTime = System.currentTimeMillis()

  case class Circle(current: Int, clockWise: Vector[Int], length: Int) {
    def insert(marble: Int): Circle = clockWise match {
      case Vector() => Circle(marble, Vector(current), length + 1)
      case _ => Circle(marble, clockWise.tail :+ current :+ clockWise.head, length + 1)
    }

    def remove: (Circle, Int) = {
      val (before, after) = (current +: clockWise).splitAt(length - 6)
      val newCircle = Circle(after.head, after.tail ++ before.init, length - 1)
      (newCircle, before.last)
    }
  }

  def play(circle: Circle, scoreBoard: Map[Int, Long], playerMarbles: Stream[(Int, Int)]): (Circle, Map[Int, Long], Stream[(Int, Int)]) = {
    if (playerMarbles.isEmpty) {
      (circle, scoreBoard, playerMarbles)
    } else {
      val (currentPlayer, marble) = playerMarbles.head
      if (marble % 23 == 0) {
        val (newCircle, removedMarble) = circle.remove
        val newScoreBoard = scoreBoard + (currentPlayer -> (scoreBoard(currentPlayer) + removedMarble + marble))
        play(newCircle, newScoreBoard, playerMarbles.tail)
      } else {
        val newCircle = circle.insert(marble)
        play(newCircle, scoreBoard, playerMarbles.tail)
      }
    }
  }

  val players = 405
  val lastMarble =  7170000

  val startingCircle = Circle(0, Vector.empty, 1)
  val scoreBoard = Map.empty[Int, Long].withDefaultValue(0l)
  val playersRepeating = Stream.continually(1 to players).flatten
  val marbles = 1 to lastMarble
  val playerMarbles: Stream[(Int, Int)] = playersRepeating.zip(marbles)

  println(play(startingCircle, scoreBoard, playerMarbles)._2.maxBy(_._2))

  val endTime = System.currentTimeMillis()
  println(s"Runtime: ${(endTime - startTime)/1000} seconds")
}
