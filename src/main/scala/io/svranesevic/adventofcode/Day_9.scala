package io.svranesevic.adventofcode

import scala.io.Source

object Day_9 extends App {

  val rope =
    Source
      .fromFile("./input/day_9.txt")
      .getLines()
      .foldLeft(Rope()) {
        case (rope, s"U $steps") => rope.up(steps.toInt)
        case (rope, s"D $steps") => rope.down(steps.toInt)
        case (rope, s"R $steps") => rope.right(steps.toInt)
        case (rope, s"L $steps") => rope.left(steps.toInt)
        case (rope, _)           => rope
      }

  val partOne = rope.tail.headPositions.distinct.size
  val partTwo = rope.knot(9).headPositions.distinct.size

  println(s"Part 1: $partOne")
  println(s"Part 2: $partTwo")

}

case class Point(x: Int, y: Int) {
  def up: Point = copy(y = y + 1)
  def down: Point = copy(y = y - 1)
  def right: Point = copy(x = x + 1)
  def left: Point = copy(x = x - 1)

  def distance(to: Point): (Int, Int) =
    (to.x - x, to.y - y)
}

case class Rope(head: Point = Point(0, 0), headPositions: List[Point] = Nil) {

  def up(steps: Int): Rope =
    if (steps < 1) this
    else this.up.up(steps - 1)

  def up: Rope = {
    val newPosition = head.up
    copy(
      head = newPosition,
      headPositions = newPosition +: headPositions
    )
  }

  def down(steps: Int): Rope =
    if (steps < 1) this
    else this.down.down(steps - 1)

  def down: Rope = {
    val newPosition = head.down
    copy(
      head = newPosition,
      headPositions = newPosition +: headPositions
    )
  }

  def right(steps: Int): Rope =
    if (steps < 1) this
    else this.right.right(steps - 1)

  def right: Rope = {
    val newPosition = head.right
    copy(
      head = newPosition,
      headPositions = newPosition +: headPositions
    )
  }

  def left(steps: Int): Rope =
    if (steps < 1) this
    else this.left.left(steps - 1)

  def left: Rope = {
    val newPosition = head.left
    copy(
      head = newPosition,
      headPositions = newPosition +: headPositions
    )
  }

  def knot(knot: Int): Rope =
    if (knot <= 0) this
    else tail.knot(knot - 1)

  def tail: Rope =
    headPositions.reverse
      .foldLeft(Rope()) { (tail, headPosition) =>
        tail.stepTowards(headPosition)
      }

  private def stepTowards(towards: Point): Rope = {
    val (xDistance, yDistance) = head.distance(towards)
    val newPosition =
      if (xDistance.abs + yDistance.abs > 2) {
        val xAdjusted = if (xDistance > 0) head.right else head.left
        val yAdjusted = if (yDistance > 0) xAdjusted.up else xAdjusted.down
        yAdjusted
      } else if (xDistance == 0 && yDistance.abs > 1) {
        if (yDistance > 0) head.up
        else head.down
      } else if (xDistance.abs > 1 && yDistance == 0) {
        if (xDistance > 0) head.right
        else head.left
      } else head

    copy(
      head = newPosition,
      headPositions = newPosition +: headPositions
    )
  }
}
