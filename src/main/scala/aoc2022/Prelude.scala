package aoc2022

import aoc2022.Prelude.Point

object Prelude {
  def packetIndex(packetMarkerSize: Int, dataStreamBuffer: String): Int = {
    val packetMarker = dataStreamBuffer.sliding(packetMarkerSize, 1).find(_.distinct.length == packetMarkerSize).get
    dataStreamBuffer.indexOf(packetMarker) + packetMarkerSize
  }

  def packet(packetMarkerSize: Int, dataStreamBuffer: String): String = {
    val packetMarker = dataStreamBuffer.sliding(packetMarkerSize, 1).find(_.distinct.length == packetMarkerSize).get
    dataStreamBuffer.split(packetMarker).last
  }

  // Returns packet marker and the packet
  def unwrap(packetMarkerSize: Int, dataStreamBuffer: String): (String, String) = {
    val packetMarker = dataStreamBuffer.sliding(packetMarkerSize, 1).find(_.distinct.length == packetMarkerSize).get
    val packet = dataStreamBuffer.split(packetMarker).last
    packetMarker -> packet
  }

  // Returns first part of the dataStreamBuffer, packet marker and the packet
  def unwrapWithRest(packetMarkerSize: Int, dataStreamBuffer: String): (String, String, String) = {
    val packetMarker = dataStreamBuffer.sliding(packetMarkerSize, 1).find(_.distinct.length == packetMarkerSize).get
    val Array(rest, packet) = dataStreamBuffer.split(packetMarker)
    (rest, packetMarker, packet)
  }

  case class Point(x: Int, y: Int)
  def fourAdjacencies(point: Point): Set[Point] = {
    val top = Point(point.x, point.y - 1)
    val left = Point(point.x - 1, point.y)
    val down = Point(point.x, point.y + 1)
    val right = Point(point.x + 1, point.y)
    Set(top, left, down, right)
  }

  def nineAdjacencies(point: Point): Set[Point] = {
    (for {
      x <- ((point.x - 1) to (point.x + 1)).toList
      y <- ((point.y - 1) to (point.y + 1)).toList
    } yield Point(x, y)).toSet
  }

  def eightAdjacencies(point: Point): Set[Point] = {
    nineAdjacencies(point).filterNot(_ == point)
  }
}
