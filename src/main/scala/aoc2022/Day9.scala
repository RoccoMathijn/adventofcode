package aoc2022

import util.AocTools
import util.InputGetter.{Example, Live, Mode}

object Day9 extends AocTools(9, 2022) {
//  implicit private val mode: Mode = Example
  implicit private val mode: Mode = Live

  case class Pos(x: Int, y: Int)
  case class History(h: List[Pos], tails: Map[String, List[Pos]])

  val s = List(Pos(0, 0))
  val tailMap = Map(
    "1" -> s,
    "2" -> s,
    "3" -> s,
    "4" -> s,
    "5" -> s,
    "6" -> s,
    "7" -> s,
    "8" -> s,
    "9" -> s
  )
  def history: History =
    inputLines.foldLeft(History(s, tailMap)) {
      case (History(h :: hx, tailMap), s"R $x") =>
        val hPath = (1 to x.toInt).map(x => h.copy(x = h.x + x)).toList
        History(hPath.reverse ++ (h :: hx), buildTMap(hPath, tailMap))
      case (History(h :: hx, tailMap), s"L $x") =>
        val hPath = (1 to x.toInt).map(x => h.copy(x = h.x - x)).toList
        History(hPath.reverse ++ (h :: hx), buildTMap(hPath, tailMap))
      case (History(h :: hx, tailMap), s"U $y") =>
        val hPath = (1 to y.toInt).map(y => h.copy(y = h.y + y)).toList
        History(hPath.reverse ++ (h :: hx), buildTMap(hPath, tailMap))
      case (History(h :: hx, tailMap), s"D $y") =>
        val hPath = (1 to y.toInt).map(y => h.copy(y = h.y - y)).toList
        History(hPath.reverse ++ (h :: hx), buildTMap(hPath, tailMap))
    }

  def buildTMap(hPath: List[Pos], tailMap: Map[String, List[Pos]]): Map[String, List[Pos]] = {
    val t1 = buildTPath(hPath, tailMap("1").head)
    val t2 = buildTPath(t1.reverse, tailMap("2").head)
    val t3 = buildTPath(t2.reverse, tailMap("3").head)
    val t4 = buildTPath(t3.reverse, tailMap("4").head)
    val t5 = buildTPath(t4.reverse, tailMap("5").head)
    val t6 = buildTPath(t5.reverse, tailMap("6").head)
    val t7 = buildTPath(t6.reverse, tailMap("7").head)
    val t8 = buildTPath(t7.reverse, tailMap("8").head)
    val t9 = buildTPath(t8.reverse, tailMap("9").head)

    Map(
      "1" -> (t1 ++ tailMap("1")),
      "2" -> (t2 ++ tailMap("2")),
      "3" -> (t3 ++ tailMap("3")),
      "4" -> (t4 ++ tailMap("4")),
      "5" -> (t5 ++ tailMap("5")),
      "6" -> (t6 ++ tailMap("6")),
      "7" -> (t7 ++ tailMap("7")),
      "8" -> (t8 ++ tailMap("8")),
      "9" -> (t9 ++ tailMap("9"))
    )
  }

  def buildTPath(predecessor: List[Pos], tStart: Pos): List[Pos] = {
    predecessor.foldLeft(List(tStart)) {
      case (lnt :: lntx, newH) =>
        (follow(newH, lnt) ++ (lnt :: lntx)).toList
    }
  }

  def follow(hPos: Pos, tPos: Pos): Option[Pos] = {
    val xDistance = math.abs(hPos.x - tPos.x)
    val yDistance = math.abs(hPos.y - tPos.y)
    (xDistance, yDistance) match {
      case (2, 0) => Some(Pos((hPos.x + tPos.x) / 2, tPos.y)) // move horizontal
      case (0, 2) => Some(Pos(tPos.x, (hPos.y + tPos.y) / 2)) // move vertically
      case (2, 1) => Some(Pos((hPos.x + tPos.x) / 2, hPos.y)) // move diagonally on x axis
      case (1, 2) => Some(Pos(hPos.x, (hPos.y + tPos.y) / 2)) // move diagonally on y axis
      case (2, 2) => Some(Pos((hPos.x + tPos.x) / 2, (hPos.y + tPos.y) / 2)) // move diagonally on both axes
      case _      => None
    }
  }

  def solve1: Int = history.tails("1").distinct.size
  def solve2: Int = history.tails("9").distinct.size

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
