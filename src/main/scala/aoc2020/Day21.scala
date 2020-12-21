package aoc2020

import util.AocTools
import util.InputGetter.{Example, Live, Mode}

import scala.annotation.tailrec

object Day21 extends AocTools(21, 2020) {
  implicit private val mode: Mode = Example
//  implicit private val mode: Mode = Live
  private val input = inputLines

  case class Food(ingredients: List[String], allergens: List[String])
  def parseLine(line: String): Food = {
    val ingredients = line.takeWhile(_ != '(').trim.split(' ').toList
    val allergens = line.dropWhile(_ != '(').drop(9).init.split(",").map(_.trim).toList
    Food(ingredients, allergens)
  }

  val allFoods: List[Food] = input.map(parseLine)

  val ingredientListPerAllergen: Map[String, List[String]] = allFoods
    .flatMap(food => food.allergens.map(allergen => allergen -> food.ingredients))
    .foldLeft(Map.empty[String, List[String]]) { case (acc, (allergen, ingredients)) => acc.updatedWith(allergen)(_.map(oldValue => oldValue.intersect(ingredients)).orElse(Some(ingredients))) }

  val ingredientsNotInAllergenList: Seq[String] = allFoods.flatMap(_.ingredients).filterNot(ingredientListPerAllergen.values.flatten.toList.contains)

  @tailrec
  def solveConstraints(ingredientListPerAllergen: Map[String, List[String]], solvedMap: Map[String, String] = Map.empty): Map[String, String] = {
    if (ingredientListPerAllergen.isEmpty) solvedMap
    else {
      val (allergensWithOneIngredient, allergensWithMoreIngredients) = ingredientListPerAllergen.partition(_._2.size == 1)
      val valuesFilteredBySolvedMap = allergensWithMoreIngredients.view.mapValues(_.filterNot(allergensWithOneIngredient.values.flatten.toList.contains)).toMap
      solveConstraints(valuesFilteredBySolvedMap, solvedMap ++ allergensWithOneIngredient.view.mapValues(_.head))
    }
  }

  def main(args: Array[String]): Unit = {
    println(s"AOC 2020 - Day $day")
    val start = System.currentTimeMillis()
    val part1 = ingredientsNotInAllergenList.size
    val mid = System.currentTimeMillis()

    println(s"Answer part 1: $part1 [${mid - start}ms]")

    val part2 = solveConstraints(ingredientListPerAllergen).toList.sortBy(_._1).map(_._2).mkString(",")
    val end = System.currentTimeMillis()
    println(s"Answer part 2: $part2 [${`end` - mid}ms]")
  }
}
