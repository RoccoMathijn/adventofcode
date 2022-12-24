package aoc2022

import aoc2022.Day19.Resources.Obsidian
import util.AocTools
import util.InputGetter.{Example, Live, Mode}

import scala.collection.parallel.CollectionConverters.ImmutableIterableIsParallelizable

object Day19 extends AocTools(19, 2022) {
//  implicit private val mode: Mode = Example
  implicit private val mode: Mode = Live

  sealed trait Resource
  object Resources {
    case object Ore extends Resource
    case object Clay extends Resource
    case object Obsidian extends Resource
    case object Geode extends Resource
  }

  sealed trait RobotType
  object RobotTypes {
    case object OreRobot extends RobotType
    case object ClayRobot extends RobotType
    case object ObsidianRobot extends RobotType
    case object GeodeRobot extends RobotType
  }

  case class Ingredient(resource: Resource, amount: Int)

  case class Blueprint(id: Int, priceList: Map[RobotType, Map[Resource, Int]]) {
    val maxMap: Map[Resource, Int] = priceList.view.values.foldLeft(Map.empty[Resource, Int])((acc, map) => mergeMapsMax(acc, map))
  }

  val blueprints = inputLines.map {
    case s"Blueprint $i: Each ore robot costs $oreCost ore. Each clay robot costs $clayOre ore. Each obsidian robot costs $obsidianOre ore and $obsidianClay clay. Each geode robot costs $geodeOre ore and $geodeObsidian obsidian." =>
      Blueprint(
        id = i.toInt,
        priceList = Map(
          RobotTypes.OreRobot -> Map(Resources.Ore -> oreCost.toInt),
          RobotTypes.ClayRobot -> Map(Resources.Ore -> clayOre.toInt),
          RobotTypes.ObsidianRobot -> Map(Resources.Ore -> obsidianOre.toInt, Resources.Clay -> obsidianClay.toInt),
          RobotTypes.GeodeRobot -> Map(Resources.Ore -> geodeOre.toInt, Resources.Obsidian -> geodeObsidian.toInt)
        )
      )
  }

  def mergeMapsMax(m1: Map[Resource, Int], m2: Map[Resource, Int]): Map[Resource, Int] = {
    (m1.toList ++ m2.toList).groupMapReduce(_._1)(_._2)(math.max)
  }

  def mergeMapsAdd(m1: Map[Resource, Int], m2: Map[Resource, Int]): Map[Resource, Int] = {
    (m1.toList ++ m2.toList).groupMapReduce(_._1)(_._2)(_+_)
  }

  case class Inventory(robots: Map[RobotType, Int], resources: Map[Resource, Int])

  def play(blueprint: Blueprint, gameStates: Set[Inventory], round: Int, maxRounds: Int): Set[Inventory] = {
    if (round > maxRounds) gameStates
    else {
      val newGameStates = gameStates.flatMap(inventory => playRound(blueprint, inventory))
      val sorted = newGameStates.map(gs => gs -> score(gs, blueprint)).toList.sortBy(_._2).reverse.map(_._1).take(1000)
      play(blueprint, sorted.toSet, round + 1, maxRounds)
    }
  }

  def overProduction(inventory: Inventory, blueprint: Blueprint): Boolean = {
    val res = blueprint.maxMap.exists {
      case (Resources.Ore, i)      => inventory.robots(RobotTypes.OreRobot) > i
      case (Resources.Clay, i)     => inventory.robots(RobotTypes.ClayRobot) > i
      case (Resources.Obsidian, i) => inventory.robots(RobotTypes.ObsidianRobot) > i
    }
    res
  }

  def enoughForGeode(inventory: Inventory, blueprint: Blueprint): Boolean = {
    val geodePriceList = blueprint.priceList(RobotTypes.GeodeRobot)
    inventory.robots(RobotTypes.OreRobot) == geodePriceList(Resources.Ore) && inventory.robots(RobotTypes.OreRobot) == geodePriceList(Resources.Obsidian)
  }

  def score(inventory: Inventory, blueprint: Blueprint): Int = {
    val orePrice = 1
    val clayPrice = blueprint.priceList(RobotTypes.ClayRobot)(Resources.Ore)
    val obsidianPrice = blueprint.priceList(RobotTypes.ObsidianRobot)(Resources.Ore) + (blueprint.priceList(RobotTypes.ObsidianRobot)(Resources.Clay) * clayPrice)
    val geodePrice = blueprint.priceList(RobotTypes.GeodeRobot)(Resources.Ore) + (blueprint.priceList(RobotTypes.GeodeRobot)(Resources.Obsidian) * obsidianPrice)


    def resourceMapToScore(resources: Map[Resource, Int]) = {
      resources.map {
        case (Resources.Ore, x)      => x * orePrice
        case (Resources.Clay, x)     => x * clayPrice
        case (Resources.Obsidian, x) => x * obsidianPrice
        case (Resources.Geode, x)    => x * geodePrice
      }.sum
    }

    val robotsWorth = inventory.robots.map {
      case (robot, i) => resourceMapToScore(blueprint.priceList(robot)) * i
    }.sum

    val resourcesWorth = resourceMapToScore(inventory.resources)

    robotsWorth + resourcesWorth
  }
  
  def playRound(blueprint: Blueprint, inventory: Inventory): Set[Inventory] = {
    val newMined: List[Resource] = inventory.robots.flatMap {
      case (RobotTypes.OreRobot, i)      => List.fill(i)(Resources.Ore)
      case (RobotTypes.ClayRobot, i)     => List.fill(i)(Resources.Clay)
      case (RobotTypes.ObsidianRobot, i) => List.fill(i)(Resources.Obsidian)
      case (RobotTypes.GeodeRobot, i)    => List.fill(i)(Resources.Geode)
    }.toList

    purchaseOptions(blueprint, inventory).map { inventory =>
      val newIngredients = newMined.foldLeft(inventory.resources)((inventory, resource) => inventory.updatedWith(resource)(x => x.map(_ + 1)))
      inventory.copy(resources = newIngredients)
    }
  }

  def isABetterthanB(inventoryA: Inventory, inventoryB: Inventory): Boolean = {
    inventoryA.robots.forall { case (robot, amount) => amount > inventoryB.robots(robot) } &&
    inventoryA.resources.forall { case (resource, amount) => amount > inventoryB.resources(resource) }
  }

  def purchase(robot: RobotType, inventory: Map[Resource, Int], blueprint: Blueprint): Map[Resource, Int] =
    blueprint.priceList(robot).foldLeft(inventory)((newInventory, ingredient) => newInventory.updatedWith(ingredient._1)(oldValue => oldValue.map(ov => ov - ingredient._2)))

  def shouldPurchase(robot: RobotType, inventory: Inventory, blueprint: Blueprint): Boolean = {
    blueprint.priceList(robot).forall {
      case (resource, cost) => inventory.resources(resource) >= cost
    } &&
    (robot match {
      case RobotTypes.OreRobot      => inventory.robots(RobotTypes.OreRobot) < blueprint.maxMap(Resources.Ore)
      case RobotTypes.ClayRobot     => inventory.robots(RobotTypes.ClayRobot) < blueprint.maxMap(Resources.Clay)
      case RobotTypes.ObsidianRobot => inventory.robots(RobotTypes.ObsidianRobot) < blueprint.maxMap(Obsidian)
      case RobotTypes.GeodeRobot    => true
    })
  }

  def purchaseOptions(bp: Blueprint, inventory: Inventory): Set[Inventory] = {
    val purchaseAbleRobots: Set[RobotType] = bp.priceList.keys.filter { robot => shouldPurchase(robot, inventory, bp) }.toSet
    if (purchaseAbleRobots.isEmpty) Set(inventory)
    else {
      purchaseAbleRobots.map { purchaseableRobot: RobotType =>
        val ingredientsAfterPurchase = purchase(purchaseableRobot, inventory.resources, bp)
        val inventoryAfterPurchase = Inventory(inventory.robots.updatedWith(purchaseableRobot)(_.map(_ + 1)), ingredientsAfterPurchase)
        inventoryAfterPurchase
      } + inventory
    }
  }

  val startState = Inventory(
    Map(RobotTypes.OreRobot -> 1, RobotTypes.ClayRobot -> 0, RobotTypes.ObsidianRobot -> 0, RobotTypes.GeodeRobot -> 0),
    Map(Resources.Ore -> 0, Resources.Clay -> 0, Resources.Obsidian -> 0, Resources.Geode -> 0)
  )


  def solve1: Int =
    blueprints
      .map(bp => {
        val states = play(bp, Set(startState), 1, 24)
        bp.id -> states.map(_.resources.getOrElse(Resources.Geode, 0)).max
      })
      .map(res => res._1 * res._2)
      .sum

  def solve2 = blueprints.par.take(3).map(play(_, Set(startState), 1, 32).map(_.resources.getOrElse(Resources.Geode, 0)).max).product
  
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
