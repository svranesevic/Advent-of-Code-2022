package io.svranesevic.adventofcode

import scala.io.Source

object Day_3 extends App {

  val partOne =
    Source
      .fromFile("./3.txt")
      .getLines()
      .map(_.toCharArray())
      .map { items =>
        val (firstCompartment, secondCompartment) = items.splitAt(items.size / 2)
        val commonItems = firstCompartment.intersect(secondCompartment).distinct
        val priorities = commonItems.flatMap(priority)
        priorities.sum
      }
      .sum

  val partTwo =
    Source
      .fromFile("./3.txt")
      .getLines()
      .map(_.toCharArray())
      .grouped(3)
      .flatMap { groupsItems =>
        val commonItem = groupsItems.reduce(_ intersect _)
        commonItem.headOption.flatMap(priority)
      }
      .sum

  def priority(item: Char): Option[Int] =
    if (item >= 'a' && item <= 'z') Some(item.toInt - 96)
    else if (item >= 'A' && item <= 'Z') Some(item.toInt - 38)
    else None

  println(s"Part 1 - Sum of common items' priorities: $partOne")
  println(s"Part 2 - Sum of badges' priorities: $partTwo")
}
