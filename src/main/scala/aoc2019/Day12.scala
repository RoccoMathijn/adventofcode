package aoc2019

import scala.collection.immutable.HashSet
import scala.io.Source

object Day12 extends App {
  val input = Source
    .fromResource("aoc2019/input-day12.txt")
    .getLines()
    .map(parseLine)
    .toList

  val initialUniverse: List[Moon] = input.map(pos => Moon(pos, Velocity.empty))

  def parseLine(line: String): Position = {
    val splitted = line.split(",")
    val xPart = splitted(0).drop(3).toInt
    val yPart = splitted(1).drop(3).toInt
    val zPart = splitted(2).init.drop(3).toInt
    Position(xPart, yPart, zPart)
  }
  case class Position(x: Int, y: Int, z: Int)
  case class Velocity(x: Int, y: Int, z: Int)
  object Velocity {
    val empty: Velocity = Velocity(0, 0, 0)
  }
  case class Moon(position: Position, velocity: Velocity)

  def simulateGravity(universe: List[Moon]): List[Moon] = {
    val universeMutable: Array[Moon] = universe.toArray
    for {
      i <- universeMutable.indices
      j <- i until universeMutable.length
    } yield {
      val (iNew, jNew) = applyGravity(universeMutable(i), universeMutable(j))

      universeMutable.update(i, iNew)
      universeMutable.update(j, jNew)
    }
    universeMutable.toList
  }

  def applyGravity(body1: Moon, body2: Moon): (Moon, Moon) =
    (body1, body2) match {
      case (
          Moon(Position(px1, py1, pz1), Velocity(vx1, vy1, vz1)),
          Moon(Position(px2, py2, pz2), Velocity(vx2, vy2, vz2))
          ) =>
        body1.copy(
          velocity = Velocity(
            newVelocity(px1, px2, vx1),
            newVelocity(py1, py2, vy1),
            newVelocity(pz1, pz2, vz1)
          )
        ) ->
          body2.copy(
            velocity = Velocity(
              newVelocity(px2, px1, vx2),
              newVelocity(py2, py1, vy2),
              newVelocity(pz2, pz1, vz2)
            )
          )
    }

  def newVelocity(p1: Int, p2: Int, vOld: Int): Int =
    (p1, p2) match {
      case _ if p1 > p2  => vOld - 1
      case _ if p1 < p2  => vOld + 1
      case _ if p1 == p2 => vOld
    }

  def simulateVelocity(universe: List[Moon]): List[Moon] =
    universe.map(addVelocityToPosition)

  def addVelocityToPosition(body: Moon): Moon =
    body.copy(
      position = Position(
        body.position.x + body.velocity.x,
        body.position.y + body.velocity.y,
        body.position.z + body.velocity.z
      )
    )

  def totalEnergy(body: Moon): Int = {
    val potentialEnergy = Math.abs(body.position.x) + Math.abs(body.position.y) + Math
      .abs(body.position.z)
    val kineticEnergy = Math.abs(body.velocity.x) + Math.abs(body.velocity.y) + Math
      .abs(body.velocity.z)
    potentialEnergy * kineticEnergy
  }

  def totalEnergySystem(universe: List[Moon]): Int =
    universe.map(totalEnergy).sum

  val step
    : List[Moon] => List[Moon] = simulateGravity _ andThen simulateVelocity

  def simulate(n: Int): List[Moon] => List[Moon] =
    Function.chain(List.fill(n)(step))

  println(totalEnergySystem(simulate(1000)(initialUniverse)))

  def countUntilCyleDetectedOnAxis(universe: List[Moon],
                                   axises: HashSet[List[(Int, Int)]],
                                   axisFun: Moon => (Int, Int),
                                   counter: Int): Int = {
    val axis = universe.map(axisFun)
    if (axises.contains(axis)) counter
    else
      countUntilCyleDetectedOnAxis(
        step(universe),
        axises + axis,
        axisFun,
        counter + 1
      )
  }

  private val xCycleCount = countUntilCyleDetectedOnAxis(
    initialUniverse,
    HashSet.empty,
    m => m.position.x -> m.velocity.x,
    0
  )
  println(s"x: $xCycleCount")
  private val yCycleCount = countUntilCyleDetectedOnAxis(
    initialUniverse,
    HashSet.empty,
    m => m.position.y -> m.velocity.y,
    0
  )
  println(s"y: $yCycleCount")
  private val zCycleCount = countUntilCyleDetectedOnAxis(
    initialUniverse,
    HashSet.empty,
    m => m.position.z -> m.velocity.z,
    0
  )
  println(s"z: $zCycleCount")

  def gcd(a: Long, b: Long): Long = if (b == 0) a.abs else gcd(b, a % b)
  def lcm(a: Long, b: Long): Long = (a * b).abs / gcd(a, b)

  println(lcm(lcm(xCycleCount, yCycleCount), zCycleCount))
}
