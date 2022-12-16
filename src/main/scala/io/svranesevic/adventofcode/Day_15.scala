package io.svranesevic.adventofcode

import scala.io.Source

object Day_15 extends App {

  val yWithRanges =
    Source
      .fromFile("./input/day_15.txt")
      .getLines()
      .map(pointAndBeaconFromString)
      .flatMap { case (sensor, beacon) =>
        Diamond.from(center = sensor, edge = beacon).area.toList
      }
      .toList
      .groupBy { case (y, _) => y }
      .map { case (y, yWithRanges) => y -> yWithRanges.map(_._2) }
      .map { case (y, ranges) => y -> Range.merge(ranges).sortBy(_.from) }

  val partOne = {
    yWithRanges.collect {
      case (y, collapsed) if y == 2_000_000L =>
        collapsed.head.to - collapsed.head.from
    }.head
  }

  val partTwo = {
    val Some(tunningFrequency) =
      yWithRanges
        .find { case (_, collapsed) =>
          collapsed.size == 2 && collapsed(0).to + 2 == collapsed(1).from
        }
        .map { case (y, left) =>
          val x = left.head.to + 1
          (4_000_000L * x) + y
        }

    tunningFrequency
  }

  println(s"Part one: $partOne")
  println(s"Part two: $partTwo")

  assert(partOne == 4737567)
  assert(partTwo == 13267474686239L)

  case class Diamond(center: Point, height: Int) {
    def contains(point: Point): Boolean = {
      math.abs(center.x - point.x) + math.abs(center.y - point.y) <= height
    }

    val area: Map[Int, Range] =
      (for {
        y <- center.y - height to center.y + height
        x = Range(center.x - height + math.abs(y - center.y), center.x + height - math.abs(y - center.y))
      } yield y -> x).toMap
  }
  object Diamond {
    def from(center: Point, edge: Point): Diamond = {
      Diamond(center, center.manhattanDistance(edge))
    }
  }

  def pointAndBeaconFromString(string: String): (Point, Point) =
    string match {
      case s"Sensor at x=${x1}, y=${y1}: closest beacon is at x=${x2}, y=${y2}" =>
        Point(x1.toInt, y1.toInt) -> Point(x2.toInt, y2.toInt)
    }

  sealed trait PointType
  case object Sensor extends PointType
  case object SensorRay extends PointType
  case object Beacon extends PointType

  case class Point(x: Int, y: Int) {
    def manhattanDistance(that: Point): Int =
      math.abs(this.x - that.x) + math.abs(this.y - that.y)
  }

  case class Range(from: Int, to: Int) {

    def contains(that: Range) = this.from <= that.from && this.to >= that.to

    def contains(element: Int) = element >= from && element <= to

    def toVector: Vector[Int] = (this.from to this.to).toVector
  }
  object Range {
    def merge(ranges: List[Range]): List[Range] =
      ranges.sortBy(_.from).foldLeft(List.empty[Range]) { (acc, range) =>
        acc match {
          case head :: tail if head.contains(range)      => head :: tail
          case head :: tail if head.contains(range.from) => Range(head.from, range.to) :: tail
          case _                                         => range :: acc
        }
      }
  }
}
