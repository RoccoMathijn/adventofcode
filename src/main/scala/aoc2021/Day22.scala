package aoc2021

import util.AocTools
import util.InputGetter.{Example, Live, Mode}

import scala.collection.mutable

object Day22 extends AocTools(22, 2021) {
//  implicit private val mode: Mode = Example
  implicit private val mode: Mode = Live

  case class RebootStep(on: Boolean, xf: Int, xt: Int, yf: Int, yt: Int, zf: Int, zt: Int)
  val input: Seq[RebootStep] = inputLines.map {
    case s"on x=$xf..$xt,y=$yf..$yt,z=$zf..$zt"  => RebootStep(true, xf.toInt, xt.toInt, yf.toInt, yt.toInt, zf.toInt, zt.toInt)
    case s"off x=$xf..$xt,y=$yf..$yt,z=$zf..$zt" => RebootStep(false, xf.toInt, xt.toInt, yf.toInt, yt.toInt, zf.toInt, zt.toInt)
  }

  val bounds: RebootStep = RebootStep(false, -50, 50, -50, 50, -50, 50)

  def withinBounds(step: RebootStep): Boolean = {
    step.xf <= bounds.xt && step.yf <= bounds.yt && step.zf <= bounds.zt &&
    step.xt >= bounds.xf && step.yt >= bounds.yf && step.zt >= bounds.zf
  }

  def filterWithIn(rebootStep: RebootStep): Option[RebootStep] = {
    if (withinBounds(rebootStep)) {
      Some(
        RebootStep(
          on = rebootStep.on,
          xf = math.max(-50, rebootStep.xf),
          xt = math.min(50, rebootStep.xt),
          yf = math.max(-50, rebootStep.yf),
          yt = math.min(50, rebootStep.yt),
          zf = math.max(-50, rebootStep.zf),
          zt = math.min(50, rebootStep.zt)
        )
      )
    } else None
  }

  def compress(cube1: RebootStep, cube2: RebootStep): Option[RebootStep] = {
    if (cube1.on == cube2.on) {
      val xAligns = cube1.xf == cube2.xf && cube1.xt == cube2.xt
      val yAligns = cube1.yf == cube2.yf && cube1.yt == cube2.yt
      val zAligns = cube1.zf == cube2.zf && cube1.zt == cube2.zt

      val xTouches = cube1.xt == cube2.xf - 1 || cube2.xt == cube1.xf - 1
      val yTouches = cube1.yt == cube2.yf - 1 || cube2.yt == cube1.yf - 1
      val zTouches = cube1.zt == cube2.zf - 1 || cube2.zt == cube1.zf - 1

      if (xAligns && yAligns && zTouches) Some(RebootStep(cube1.on, cube1.xf, cube1.xt, cube1.yf, cube1.yt, math.min(cube1.zf, cube2.zf), math.max(cube1.zt, cube2.zt)))
      else if (xAligns && zAligns && yTouches) Some(RebootStep(cube1.on, cube1.xf, cube1.xt, math.min(cube1.yf, cube2.yf), math.max(cube1.yt, cube2.yt), cube1.zf, cube1.zt))
      else if (yAligns && zAligns && xTouches) Some(RebootStep(cube1.on, math.min(cube1.xf, cube2.xf), math.max(cube1.xt, cube2.xt), cube1.yf, cube1.yt, cube1.zf, cube1.zt))
      else
        None
    } else None
  }

  def compressSet(compressed: Set[RebootStep] = Set.empty, doing: List[RebootStep] = List.empty, todo: List[RebootStep]): Set[RebootStep] = {
    if (doing.isEmpty && todo.isEmpty) compressed
    else if (todo.isEmpty && doing.size == 1) compressed + doing.head
    else if (todo.isEmpty) compressSet(compressed + doing.head, List(doing.drop(1).head), doing.drop(2))
    else if (doing.isEmpty) compressSet(compressed, List(todo.head), todo.tail)
    else {
      val maybeCompressed = compress(doing.head, todo.head)
      compressSet(compressed, maybeCompressed.map(cStep => cStep :: doing.tail).getOrElse(doing :+ todo.head), todo.tail)
    }
  }

  case class Cube(x: Int, y: Int, z: Int)
  def apply(rebootStep: RebootStep, onCubes: Set[Cube] = Set.empty): Set[Cube] = {
    val cubes = for {
      x <- rebootStep.xf to rebootStep.xt
      y <- rebootStep.yf to rebootStep.yt
      z <- rebootStep.zf to rebootStep.zt
    } yield Cube(x, y, z)

    if (rebootStep.on) onCubes ++ cubes else onCubes -- cubes
  }

  def completelyWithin(rebootStep1: RebootStep, rebootStep2: RebootStep): Boolean = {
    rebootStep1.xf >= rebootStep2.xf &&
    rebootStep1.xt <= rebootStep2.xt &&
    rebootStep1.yf >= rebootStep2.yf &&
    rebootStep1.yt <= rebootStep2.yt &&
    rebootStep1.zf >= rebootStep2.zf &&
    rebootStep1.zt <= rebootStep2.zt
  }

  def splitCube(step1: RebootStep, step2: RebootStep): Set[RebootStep] = {
    val splitted: Set[RebootStep] = for {
      x <- splitLine(Line(step1.xf, step1.xt), Line(step2.xf, step2.xt))
      y <- splitLine(Line(step1.yf, step1.yt), Line(step2.yf, step2.yt))
      z <- splitLine(Line(step1.zf, step1.zt), Line(step2.zf, step2.zt))
    } yield RebootStep(true, x.from, x.to, y.from, y.to, z.from, z.to)

    val step1Cubes = splitted.filter(c => completelyWithin(c, step1)).filterNot(c => completelyWithin(c, step2))
    compressSet(todo = step1Cubes.toList)
  }

  case class Line(from: Int, to: Int)
  def splitLine(line: Line, line2: Line): Set[Line] = {
//    println(s"Splitting $line and $line2")
    val sorted = List(line.from, line.to + 1, line2.from, line2.to + 1).sorted
    Set(
      Line(
        sorted(0),
        sorted(1) - 1
      ),
      Line(
        sorted(1),
        sorted(2) - 1
      ),
      Line(
        sorted(2),
        sorted(3) - 1
      )
    )
  }.filter(ln => ln.from <= ln.to)

  def count(step: RebootStep): Long = {
    val x = math.abs(step.xt - step.xf) + 1
    val y = math.abs(step.yt - step.yf) + 1
    val z = math.abs(step.zt - step.zf) + 1
    x.toLong * y.toLong * z.toLong
  }
  def countCubes(steps: Set[RebootStep]): Long = {
    steps.foldLeft(0L)((acc, step) => acc + count(step))
  }

  def applyAll: Set[Cube] = input.flatMap(filterWithIn).foldLeft(Set.empty[Cube])((cubes, step) => apply(step, cubes))

  def applyAll2: Set[RebootStep] =
    input.tail.foldLeft(Set(input.head)) { (onCubes, step) =>
      println(s"${input.zipWithIndex.find(_._1 == step).get._2 + 1}/${input.size}")
      val splitted = onCubes.flatMap(onCube => splitCube(onCube, step))
      if (step.on)
        splitted + step
      else splitted
    }

  def solve1: Long = applyAll.size
  def solve2: Long = countCubes(applyAll2)

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
