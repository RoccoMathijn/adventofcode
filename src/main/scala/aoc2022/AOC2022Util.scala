package aoc2022

object AOC2022Util {
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
}
