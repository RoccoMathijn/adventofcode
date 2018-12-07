package aoc2018

import scala.io.Source

object Day7 extends App {
  val startTime = System.currentTimeMillis()

  case class Edge(from: Char, to: Char)
  type Graph = Seq[Edge]

  val steps: Graph = Source
    .fromResource("input-day7.txt")
    .getLines
    .map(parseLine)
    .toList

  def parseLine(rawString: String): Edge = {
    Edge(rawString.charAt(5), rawString.charAt(36))
  }

  def findFirstStep(graph: Graph): Char = {
    graph
      .filter { case Edge(from, _) => !graph.exists { case Edge(_, to) => from == to } }
      .map(_.from)
      .min
  }

  def determineOrder(remainingGraph: Graph, order: String): String = {
    remainingGraph match {
      case Nil =>
        val lastStep = remainingGraph.find { case Edge(from, _) => from == order.last }.map(_.to).get
        order :+ lastStep
      case List(step) => order :+ step.from :+ step.to
      case ss =>
        val nextStep = findFirstStep(ss)
        val remainingSteps = ss.filterNot { case Edge(from, _) => from == nextStep }
        determineOrder(remainingSteps, order :+ nextStep)
    }
  }

  println(s"Answer part1: ${determineOrder(steps, "")}")

  val MIN_STEP_DURATION = 60
  val WORKERS = 5

  case class Task(step: Char, startTime: Int) {
    val duration: Int = step.toInt - 64 + MIN_STEP_DURATION
  }

  val allSteps = (steps.map(_.from) ++ steps.map(_.to)).distinct

  def findAvailableSteps(graph: Graph, remainingTasks: Seq[Char]): Seq[Char] = {
    if (graph.isEmpty) {
      remainingTasks
    } else {
      graph
        .filterNot { case Edge(from, _) => graph.exists { case Edge(_, to) => from == to } }
        .map(_.from)
    }
  }

  def determineDurationWithMultipleWorkers(remainingGraph: Graph, remainingSteps: Seq[Char], taskList: Set[Task], seconds: Int): Int = {
    val finishedTasks: Set[Task] = taskList.filter(task => task.startTime + task.duration == seconds)
    val newRemainingSteps = remainingSteps.filterNot(task => finishedTasks.exists(_.step == task))

    if (newRemainingSteps.isEmpty) {
      seconds
    } else {
      val currentTaskList: Set[Task] = taskList -- finishedTasks

      val newRemainingGraph: Graph = remainingGraph.filterNot(step => finishedTasks.exists(_.step == step.from))
      val stepsThatCanBeStarted: Seq[Char] = findAvailableSteps(newRemainingGraph, remainingSteps)
        .filterNot(step => taskList.exists(_.step == step))
        .distinct

      val availableWorkers: Int = (WORKERS - currentTaskList.size).max(0)
      val newTaskList: Set[Task] = currentTaskList ++ stepsThatCanBeStarted.take(availableWorkers).map(Task(_, seconds))

      determineDurationWithMultipleWorkers(newRemainingGraph, newRemainingSteps, newTaskList, seconds + 1)
    }
  }

  println(s"Answer part2: ${determineDurationWithMultipleWorkers(steps, allSteps, Set.empty, 0)}")

  val endTime = System.currentTimeMillis()
  println(s"Runtime: ${endTime - startTime}")
}
