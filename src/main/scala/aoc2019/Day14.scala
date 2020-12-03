package aoc2019

import scala.io.Source

object Day14 extends App {
  val reactionList: List[(List[Product], Product)] = Source
    .fromResource("aoc2019/input-day14.txt")
    .getLines()
    .map(parseLine)
    .toList

  type Chemical = String
  type Product = (Long, Chemical)
  type Reaction = (List[Product], Product)

  def parseLine(line: String): Reaction = {
    val splitted = line.split("=>")
    val chemicalList = splitted(0)
    val result = splitted(1).trim.split(' ')
    val chemicals: List[Product] = chemicalList
      .split(',')
      .map(_.trim)
      .map(_.split(' '))
      .map(x => x(0).toLong -> x(1))
      .toList
    chemicals -> (result(0).toInt, result(1))
  }

  def findIngredientsFor(product: Product): (List[Product], Product) = {
    val reaction: Reaction = reactionList.find(_._2._2 == product._2).get
    val multiplier = closestMultiplierThatIsLarger(reaction._2._1, product._1, 1)
    val result = multiplyProducts(reaction._1, multiplier)
    val waste = (reaction._2._1 * multiplier) - product._1 -> product._2
    //    println(s"Consume ${result.map(t => s"${t._1} ${t._2}").mkString(", ")} to produce ${product._1} ${product._2}")
    //    println(s"Wasting ${waste._1} ${waste._2}")
    result -> waste
  }

  def closestMultiplierThatIsLarger(a: Long, b: Long, acc: Long): Long =
    if (a * acc >= b) acc else closestMultiplierThatIsLarger(a, b, acc + 1)

  def multiplyProducts(list: List[Product], multiplier: Long): List[Product] =
    list.map(x => x._1 * multiplier -> x._2)

  def flatten(list: List[List[Product]]): List[(Long, Chemical)] = list.flatten.groupBy(_._2).mapValues(_.map(_._1).sum).toList.map(_.swap)

  def subtractFromWaste(neededProducts: List[Product], waste: List[Product]): (List[Product], List[Product]) = {
    val (newNeededProducts, newWaste) = flatten(List(neededProducts.map(t => -t._1 -> t._2), waste)).partition(_._1 < 0)
    newNeededProducts.map(t => t._1.abs -> t._2) -> newWaste.filter(_._1 > 0)
  }

  def findRecursive(ingredients: List[Product], waste: List[Product], ore: Long): (Long, List[Product]) = {
    val (newIngredients, newWaste) = subtractFromWaste(ingredients, waste)
    val (chems, ores) = newIngredients.partition(_._2 != "ORE")
    val newOre = ores.map(_._1).sum + ore
    chems match {
      case Nil => (newOre, newWaste)
      case ing => val res = ing.map(findIngredientsFor)
        val wastes = res.map(_._2)
        val newIngs = flatten(res.map(_._1))
        findRecursive(newIngs, flatten(List(newWaste, wastes)), newOre)
    }
  }

  //  println(findRecursive(List(1l -> "FUEL"), List.empty, 0))

  def untilNoWaste(waste: List[Product], oldOre: Long, counter: Int, wasteLog: Vector[List[Product]]): (Long, Int) = {
//    println(s"counter: $counter")
    val (newOre, newWaste) = findRecursive(List(1l -> "FUEL"), waste, 0)
    if (newWaste.isEmpty) (newOre + oldOre, counter)
    else untilNoWaste(newWaste, newOre + oldOre, counter + 1, waste +: wasteLog)
  }

  def untilNoMoreOre(waste: List[Product], oldOre: Long, fuel: Long): Long = {
        println(s"Ore: $oldOre, fuel: $fuel")
    val (usedOre, leftOverWaste) = findRecursive(List(1l -> "FUEL"), waste, 0)
    if (usedOre > oldOre) fuel
    else untilNoMoreOre(leftOverWaste, oldOre - usedOre, fuel + 1)
  }

//  private val cyclesUntilNoWaste: (Long, Int) = untilNoWaste(List.empty, 0, 1, Vector.empty)
//  private val orePerCycle: Long = cyclesUntilNoWaste._1
//  private val fuelPerCycle: Int = cyclesUntilNoWaste._2
//  println(s"Fuel in 1 cycle: ${fuelPerCycle} Total ore in cycle: ${orePerCycle}")
//  private val howManyCycles: Long = 1000000000000l / orePerCycle
//  val leftOverOre = 1000000000000l - (howManyCycles.toLong * orePerCycle.toLong)
//  println(s"Ore left after $howManyCycles cycles: ${leftOverOre}")
//  private val fuelHarvestedFromCompletedCycles: Long = 1000000000000l / orePerCycle * fuelPerCycle
//  private val fuelHarvestedFromLeftOverOre: Long = untilNoMoreOre(List.empty, leftOverOre, 0l)
//  println(s"Total ore harvested: ${fuelHarvestedFromCompletedCycles + fuelHarvestedFromLeftOverOre}")


  val oreInCargo = 1000000000000l
  def untilMoreThanTrillion(waste: List[Product], fuel: Long, lowerBound: Long, upperBound: Long): Long = {
    if (fuel - lowerBound < 1 || upperBound - fuel < 1) {
      println(s"Answer $fuel")
      fuel
    } else
    {
      println(s"Trying with fuel: $fuel and lowerBound: $lowerBound, upperBound: $upperBound")
      val (requiredsOre, newWaste) = findRecursive(List(fuel -> "FUEL"), waste, 0)
      println(requiredsOre)
      if (requiredsOre > 1000000000000l) {
        untilMoreThanTrillion(newWaste, (fuel + lowerBound) / 2, lowerBound, fuel)
      }
      else {
        untilMoreThanTrillion(newWaste, (fuel + upperBound) / 2, fuel, upperBound)
      }
    }
  }


  private val upperBound = 100000000
  println(untilMoreThanTrillion(List.empty, upperBound / 2, 0, upperBound))

}