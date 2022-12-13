package io.svranesevic.adventofcode

import scala.io.Source
import scala.annotation.tailrec

object Day_12 extends App {

  val terrain =
    Source
      .fromFile("./input/day_12.txt")
      .getLines()
      .map(_.toVector)
      .toVector

  val startPoint = {
    val y = terrain.indexWhere(_.contains('S'))
    val x = terrain(y).indexWhere(_ == 'S')
    Point(x, y)
  }

  val endPoint = {
    val y = terrain.indexWhere(_.contains('E'))
    val x = terrain(y).indexWhere(_ == 'E')
    Point(x, y)
  }

  case class Point(x: Int, y: Int) {

    def neighbours: Vector[Point] =
      Vector(copy(x = x + 1), copy(x = x - 1), copy(y = y + 1), copy(y = y - 1))
  }

  def breadthFirst(
      from: Point,
      goalReached: Point => Boolean,
      canReach: (Point, Point) => Boolean
  ): Option[Vector[Point]] = {
    val queue = scala.collection.mutable.Queue(from -> Vector.empty[Point])
    val visited = scala.collection.mutable.Set.empty[Point]

    while (queue.nonEmpty) {
      val (current, steps) = queue.dequeue()
      if (!visited.contains(current)) {
        visited.add(current)
        if (goalReached(current)) return Some(steps :+ current)
        else
          current.neighbours
            .filter(next => canReach(current, next))
            .foreach(point => queue.enqueue(point -> (steps :+ current)))
      }
    }

    None
  }

  def canReach(from: Point, to: Point): Boolean = {
    val canReach =
      for {
        fromHeight <- heightOf(from)
        toHeight <- heightOf(to)
      } yield toHeight + 1 >= fromHeight

    canReach.getOrElse(false)
  }

  def heightOf(point: Point): Option[Int] =
    for {
      row <- terrain.lift(point.y)
      char <- row.lift(point.x)
      height = char match {
        case 'E' => 25
        case 'S' => 0
        case o   => o - 97
      }
    } yield height

  def partOneGoal(point: Point): Boolean = point == startPoint
  val Some(partOnePath) = breadthFirst(endPoint, partOneGoal, canReach)
  val partOne = partOnePath.length - 1

  def partTwoGoal(point: Point): Boolean = heightOf(point) == Some(0)
  val Some(partTwoPath) = breadthFirst(endPoint, partTwoGoal, canReach)
  val partTwo = partTwoPath.length - 1

  println(s"Part 1: $partOne")
  println(s"Part 2: $partTwo")
}
