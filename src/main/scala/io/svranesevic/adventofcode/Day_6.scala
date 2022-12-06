package io.svranesevic.adventofcode

import scala.io.Source

object Day_6 extends App {

  val dataStream =
    Source
      .fromFile("./6.txt")
      .mkString
      .toCharArray()

  val Some(partOne) =
    (0 until dataStream.length).find { index =>
      isMarker(dataStream, atIndex = index, markerLength = 4)
    }

  val Some(partTwo) =
    (0 until dataStream.length).find { index =>
      isMarker(dataStream, atIndex = index, markerLength = 14)
    }

  println(s"Part one: $partOne")
  println(s"Part two: $partTwo")

  def isMarker(dataStream: Array[Char], atIndex: Int, markerLength: Int): Boolean = {
    val potentialMarker = dataStream.slice(atIndex - markerLength, atIndex)
    val marker = potentialMarker.distinct
    marker.length == markerLength
  }
}
