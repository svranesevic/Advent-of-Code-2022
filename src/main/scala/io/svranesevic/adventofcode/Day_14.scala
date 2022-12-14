package io.svranesevic.adventofcode

import scala.io.Source
import scala.annotation.tailrec

object Day_14 extends App {

  val cave =
    Source
      .fromFile("./input/day_14.txt")
      .getLines()
      .flatMap { lines =>
        val edges = lines.split(" -> ")
        val points = edges.map(Point.fromString)
        points.sliding(2).flatMap { case Array(left, right) =>
          pointsBetween(left, right)
        }
      }
      .map(_ -> Wall)
      .toMap

  val partOne = simulate(cave).values.count(_ == Sand)
  val partTwo = {
    val highestY = cave.keySet.map(_.y).max
    simulateWithFloor(cave, floorY = highestY + 2).values.count(_ == Sand)
  }
  println(s"Part 1: $partOne")
  println(s"Part 2: $partTwo")

  @tailrec
  def simulate(cave: Map[Point, PointType]): Map[Point, PointType] = {
    @tailrec
    def step(sand: Point, cave: Map[Point, PointType]): Map[Point, PointType] = {
      if (sand.y >= cave.keySet.map(_.y).max) cave
      else if (!cave.contains(sand.down)) step(sand.down, cave)
      else if (!cave.contains(sand.down.left)) step(sand.down.left, cave)
      else if (!cave.contains(sand.down.right)) step(sand.down.right, cave)
      else cave.updated(sand, Sand)
    }

    val sand = Point(x = 500, y = 0)
    val newCave = step(sand, cave)
    if (newCave == cave) cave
    else simulate(newCave)
  }

  @tailrec
  def simulateWithFloor(cave: Map[Point, PointType], floorY: Int): Map[Point, PointType] = {
    def occupied(cave: Map[Point, PointType], point: Point): Boolean = {
      point.y >= floorY || cave.contains(point)
    }

    @tailrec
    def step(sand: Point, cave: Map[Point, PointType]): Map[Point, PointType] = {
      if (!occupied(cave, sand.down)) step(sand.down, cave)
      else if (!occupied(cave, sand.down.left)) step(sand.down.left, cave)
      else if (!occupied(cave, sand.down.right)) step(sand.down.right, cave)
      else cave.updated(sand, Sand)
    }

    val sand = Point(x = 500, y = 0)
    val newCave = step(sand, cave)
    if (newCave == cave) cave
    else simulateWithFloor(newCave, floorY)
  }

  def pointsBetween(left: Point, right: Point): Vector[Point] =
    (left, right) match {
      case (left, right) if left.x == right.x =>
        val startY = math.min(left.y, right.y)
        val endY = math.max(left.y, right.y)
        (startY to endY).map(y => Point(left.x, y)).toVector

      case (left, right) if left.y == right.y =>
        val startX = math.min(left.x, right.x)
        val endX = math.max(left.x, right.x)

        (startX to endX).map(x => Point(x, left.y)).toVector

      case _ =>
        Vector(left, right)
    }

  sealed trait PointType
  case object Sand extends PointType
  case object Wall extends PointType

  case class Point(x: Int, y: Int) {
    def up: Point = copy(y = y - 1)
    def down: Point = copy(y = y + 1)
    def right: Point = copy(x = x + 1)
    def left: Point = copy(x = x - 1)
  }
  object Point {
    def fromString(string: String): Point = {
      val Array(x, y) = string.split(",")
      Point(x.toInt, y.toInt)
    }
  }
}
