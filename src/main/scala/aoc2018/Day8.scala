package aoc2018

import scala.io.Source

object Day8 extends App {
  val startTime = System.currentTimeMillis()

  val licenceFile: List[Int] = Source
    .fromResource("aoc2018/input-day8.txt")
    .mkString
    .split(' ')
    .map(_.toInt)
    .toList


  case class Node(childNodes: Seq[Node], metaData: Seq[Int]) {
    val metaDataSum: Int = metaData.sum + childNodes.map(_.metaDataSum).sum

    val value: Int = {
      if (childNodes.isEmpty) metaDataSum
      else {
        val childNodesWithIndex = childNodes.zipWithIndex
        metaData.flatMap { index =>
          childNodesWithIndex.find { case (_, nodeIndex) => (index - 1) == nodeIndex }
        }.map(_._1.value).sum
      }
    }
  }

  def parse(licenceFile: Seq[Int]): (Node, Seq[Int]) = {
    licenceFile match {
      case 0 :: qmd :: lf =>
        val (metaData, rem) = lf.splitAt(qmd)
        (Node(Seq.empty, metaData), rem)
      case c :: qmd :: lf =>
        val (nodes, rem) = parseHorizontal(lf, c)
        val (metaData, rem2) = rem.splitAt(qmd)
        (Node(nodes, metaData), rem2)
    }
  }

  def parseHorizontal(licenceFile: Seq[Int], children: Int): (Seq[Node], Seq[Int]) = {
    if (children == 0) {
      (Seq.empty, licenceFile)
    } else {
      val (nodes, rem) = parse(licenceFile)
      val (nodes2, rem2) = parseHorizontal(rem, children - 1)
      (nodes +: nodes2, rem2)
    }
  }

  val tree: Node = parseHorizontal(licenceFile, 1)._1.head

  println(s"Answer part 1: ${tree.metaDataSum}")

  println(s"Answer part 2: ${tree.value}")

  val endTime = System.currentTimeMillis()
  println(s"Runtime: ${endTime - startTime}")
}
