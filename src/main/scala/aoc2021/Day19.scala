package aoc2021

import util.AocTools
import util.InputGetter.{Live, Mode}

object Day19 extends AocTools(19, 2021) {
//  implicit private val mode: Mode = Example
  implicit private val mode: Mode = Live

  def parse(input: String): List[Scanner] = {
    input.split("\n\n").map { scanner =>
      val scannerLines = scanner.split("\n")
      val n = scannerLines.head match {
        case s"--- scanner $n ---" => n.toInt
      }
      val beacons = scannerLines.tail.map { line =>
        val coordinates = line.split(',')
        Point(coordinates(0).toInt, coordinates(1).toInt, coordinates(2).toInt)
      }.toList
      Scanner(n, beacons)
    }
  }.toList

  def scanners: List[Scanner] = parse(inputBlob)

  def align(beacons: List[Point], point: Point): List[Point] = beacons.map(b => Point(b.x + point.x, b.y + point.y, b.z + point.z))

  def seeSameBeacons(scanner1: Scanner, scanner2: Scanner): Option[(Scanner, Point)] = {
    val rotations = scannerRotations(scanner2)
    rotations.flatMap { rotation =>
      val alignmentPoint = findAlignmentPoint(scanner1, rotation)
      if (alignmentPoint.isDefined) println(s"Scanner ${scanner1.n} overlaps with ${scanner2.n}")
      alignmentPoint.map(ap => Scanner(scanner2.n, rotation) -> ap)
    }.headOption
  }

  private def findAlignmentPoint(scanner: Scanner, rotation: List[Point]): Option[Point] = {
    for {
      xDiff <- alignAlongAxis(scanner.beacons.map(_.x), rotation.map(_.x))
      yDiff <- alignAlongAxis(scanner.beacons.map(_.y), rotation.map(_.y))
      zDiff <- alignAlongAxis(scanner.beacons.map(_.z), rotation.map(_.z))
    } yield Point(xDiff, yDiff, zDiff)
  }

  def alignAlongAxis(list1: List[Int], list2: List[Int]): Option[Int] = {
    val ds = for {
      x <- list1
      y <- list2
    } yield math.abs(x - y)
    ds.toSet.find(d => (list2.map(_ + d) ++ list2.map(_ - d)).intersect(list1).size >= 12)
  }

  def scan(done: List[(Int, (Scanner, Point))], found: List[(Int, (Scanner, Point))], todo: List[Scanner]): Cluster = {
    println(s"To scan: ${todo.size}")
    if (todo.isEmpty) done ++ found
    else {
      val (_, (scanner, _)) = found.head
      println(s"Finding pairs for: ${scanner.n}")
      val paired = todo.flatMap(scanner2 => seeSameBeacons(scanner, scanner2).map(s2 => scanner.n -> s2))
      if (paired.nonEmpty) println(s"Paired: ${paired.map(_._2._1.n).mkString(",")}")
      val leftOver = todo.filterNot(s => paired.map(_._2._1.n).contains(s.n))
      scan(found.head :: done, found.tail ++ paired, leftOver)
    }
  }

  type Cluster = List[(Int, (Scanner, Point))]
  def run(scanners: List[Scanner]): Cluster = scan(List.empty, List(0 -> (scanners.head, Point(0, 0, 0))), scanners.tail)

  def clusterBeacons(cluster: Cluster): Set[Point] = cluster.map(_._2._1).flatMap(s => align(s.beacons, resolveAlignmentFromS0(s.n, cluster).reduce(plus))).toSet

  def pointsFromS0(cluster: Cluster): List[Point] = cluster.map(_._2._1).map(s => resolveAlignmentFromS0(s.n, cluster).reduce(plus))

  def pairs(list: List[Point]): Seq[(Point, Point)] =
    for {
      x <- list.indices
      y <- list.indices
      if x != y
    } yield (list(x), list(y))

  def manhattanDistance(point1: Point, point2: Point): Int = math.abs(point1.x - point2.x) + math.abs(point1.y - point2.y) + math.abs(point1.z - math.abs(point2.z))

  def resolveAlignmentFromS0(n: Int, cluster: Cluster): List[Point] = {
    if (n == 0) List(Point(0, 0, 0))
    else {
      cluster
        .find(_._2._1.n == n)
        .map { res =>
          resolveAlignmentFromS0(res._1, cluster) :+ res._2._2
        }
        .get
    }
  }

  def plus(point1: Point, point2: Point): Point = Point(point1.x + point2.x, point1.y + point2.y, point1.z + point2.z)

  case class Scanner(n: Int, beacons: List[Point])
  case class Point(x: Int, y: Int, z: Int)

  def scannerRotations(scanner: Scanner): List[List[Point]] = scanner.beacons.map(rotations).transpose.distinct

  def rotations(point: Point): List[Point] =
    for {
      x <- rotateXAxis(point)
      y <- rotateYAxis(x)
      z <- rotateZAxis(y)
    } yield z

  def rotateXAxis(point: Point): List[Point] = {
    List(
      Point(point.x, point.y, point.z),
      Point(point.x, point.z, -point.y),
      Point(point.x, -point.y, -point.z),
      Point(point.x, -point.z, point.y)
    )
  }

  def rotateZAxis(point: Point): List[Point] = {
    List(
      Point(point.x, point.y, point.z),
      Point(point.y, -point.x, point.z),
      Point(-point.x, -point.y, point.z),
      Point(-point.y, point.x, point.z)
    )
  }

  def rotateYAxis(point: Point): List[Point] = {
    List(
      Point(point.x, point.y, point.z),
      Point(point.z, point.y, -point.x),
      Point(-point.x, point.y, -point.z),
      Point(-point.z, point.y, point.x)
    )
  }

  def solve1: Int = clusterBeacons(run(scanners)).size
  def solve2: Int = pairs(pointsFromS0(run(scanners))).map(pair => manhattanDistance(pair._1, pair._2)).max

  def main(args: Array[String]): Unit = {
    val start = System.currentTimeMillis()
    println(s"AOC $year - Day $day")

    val part1 = solve1

    val mid = System.currentTimeMillis()
    println(s"Answer part 1: $part1 [${mid - start}ms]")

    val part2 = solve2
    val end = System.currentTimeMillis()
    println(s"Answer part 2: $part2 [${end - mid}ms]")
  }
}
