import scala.io.Source

object Day1Part1 extends App {
  val result = Source.fromResource("input-day1.txt").getLines().map(Integer.parseInt).sum
  println(result)
}
