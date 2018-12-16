package aoc2018

object Day14 extends App {
  val input: Seq[Char] = "681901".toVector

  val recipes: Vector[Char] = Vector('3', '7')

  def brew(recipes: Seq[Char], elf1: Int, elf2: Int): String = {
    val elf1RecipeScore = recipes(elf1).toString.toInt
    val elf2RecipeScore = recipes(elf2).toString.toInt
    val sum = (elf1RecipeScore + elf2RecipeScore).toString.toVector
    val newRecipes = recipes ++ sum
    val newRecipesLength = newRecipes.length
    if (newRecipesLength > input.mkString.toInt + 10) {
      newRecipes.slice(input.mkString.toInt, input.mkString.toInt + 10).mkString
    } else {
      val newElf1Index = (elf1 + (elf1RecipeScore + 1)) % newRecipesLength
      val newElf2Index = (elf2 + (elf2RecipeScore + 1)) % newRecipesLength
      brew(newRecipes, newElf1Index, newElf2Index)
    }
  }

  // Answer Part 1
  println(brew(recipes, 0, 1))

  def brewPart2(recipes: Seq[Char], elf1: Int, elf2: Int): Int = {
    val elf1RecipeScore = recipes(elf1).toString.toInt
    val elf2RecipeScore = recipes(elf2).toString.toInt
    val sum = (elf1RecipeScore + elf2RecipeScore).toString.toVector
    val newRecipes = recipes ++ sum
    val newRecipesLength = newRecipes.length
    println(newRecipesLength)

    if (newRecipes.takeRight(input.length + 1).sliding(input.length).contains(input)) {
      if (sum.length == 2) {
        newRecipes.length - input.length - 1
      } else {
        newRecipes.length - input.length
      }
    } else {
      val newElf1Index = (elf1 + (elf1RecipeScore + 1)) % newRecipesLength
      val newElf2Index = (elf2 + (elf2RecipeScore + 1)) % newRecipesLength
      brewPart2(newRecipes, newElf1Index, newElf2Index)
    }
  }

  // Answer Part 2
  println(brewPart2(recipes, 0, 1))
}
