//package aoc2018
//
//object Day11 extends App {
//  val startTime = System.currentTimeMillis()
//
//  val gridSerialNumber: Int = 7139
//
//  def powerLevel(x: Int, y: Int) = {
//    val rackId = x + 10
//    val powerLevel = (rackId * y + gridSerialNumber) * rackId
//
//    val rem = powerLevel % 1000
//    val rem2 = rem % 100
//    (rem - rem2) % 99 - 5
//  }
//
//  def subGrid(x: Int, y: Int, size: Int): Seq[(Int, Int)] = {
//    for {
//      j <- y until (y + size)
//      i <- x until (x + size)
//    } yield (i, j)
//  }
//
//  def topLeftCoordinates(size: Int): Seq[(Int, Int)] = for {
//    y <- 1 to (300 - size + 1)
//    x <- 1 to (300 - size + 1)
//  } yield (x, y)
//
//  val powerLevelGrid: Map[(Int, Int), Int] = (for {
//    y <- 1 to 300
//    x <- 1 to 300
//  } yield (x,y) -> powerLevel(x, y)).toMap
//
//  val summedTable: Map[(Int, Int), Int]  = for {
//    y <- 1 to 300
//    x <- 1 to 300
//  } yield (x,y) ->
//
//  def toSummedTable(powerLevelGrid: Map[(Int, Int), Int]) = {
//
//  }
//
//  def topLeftCoordinateWithHighestSubgridForSize(size: Int): (Int, Int) = topLeftCoordinates(size).maxBy(c => subGrid(c._1, c._2, 3).map(c => powerLevel(c._1, c._2)).sum)
//
//  println(subGrid(20,62,3).map(c => powerLevel(c._1, c._2)).sum)
//  println(topLeftCoordinateWithHighestSubgridForSize(3))
//  def getSumLastRowColumn(x: Int, y: Int, size: Int): Int = {
//    val columnIndex = x + size - 1
//    val rowIndex = y + size - 1
//    val xx = (y to rowIndex).map(newY => (columnIndex, newY))
//    val yy = (x until columnIndex).map(newX => (newX, rowIndex))
//    (xx ++ yy).map(c => powerLevelGrid(c._1, c._2)).sum
//  }
//
//  val topLeftCoordinateWithHighestSubgridPart2: (Int, Int, (Int, Long)) = (for {
//    y <- 1 to 300
//    x <- 1 to 300
//  } yield {
//    val sizeTotal: (Int, Long) = (1 to (300 - x.max(y) + 1).scanLeft((0,0l))((rt, size) => {
//      (size, rt._2 + getSumLastRowColumn(x, y, size))
//    }).maxBy(_._2)
//    println(s"$x, $y, $sizeTotal")
//    (x, y, sizeTotal)
//  }).maxBy(_._3._2)
//  println(topLeftCoordinateWithHighestSubgridPart2)
//
//  val endTime = System.currentTimeMillis()
//  println(s"Runtime: ${(endTime - startTime)/1000} seconds")
//}
