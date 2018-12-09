package aoc2018

import scala.io.Source

object Day03 extends App {
  val claims = Source.fromResource("aoc2018/input-day3.txt").getLines().toList.map(parseLine)

  case class Claim(id: Int, x: Int, y: Int, length: Int, width: Int)

  def parseLine(line: String) = {
    val splitted = line.split(' ')

    val id = Integer.parseInt(splitted.head.tail)
    val Array(x, y) = splitted(2).init.split(',').map(Integer.parseInt)
    val Array(width, length) = splitted(3).split('x').map(Integer.parseInt)

    Claim(id, x, y, length, width)
  }

  def toCoordinates(claim: Claim): Seq[(Int, Int)] = {
    val xs = claim.x + 1 to claim.width + claim.x
    val ys = claim.y + 1 to claim.length + claim.y

    for {
      x <- xs
      y <- ys
    } yield (x, y)
  }

  // Answer part 1
  println(
    claims
    .flatMap(toCoordinates)
    .groupBy(identity)
    .values
    .filter(_.length >= 2)
    .toList
    .length
  )

  val claimsWithDuplicates: Seq[Claim] = claims
    .flatMap(claim => toCoordinates(claim).map(pos => claim -> pos))
    .groupBy(_._2)
    .values
    .filter(_.length > 1)
    .flatMap(_.map(_._1))
    .toList

  // Answer part 2
  println(claims.filter(claim => !claimsWithDuplicates.contains(claim)))
}
