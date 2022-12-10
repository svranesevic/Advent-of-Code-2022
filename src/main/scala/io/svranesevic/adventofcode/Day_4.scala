package io.svranesevic.adventofcode

import scala.io.Source

object Day_4 extends App {

  val partOne =
    Source
      .fromFile("./input/day_4.txt")
      .getLines()
      .map(_.split(","))
      .count { sectionAssignments =>
        val firstElf =
          sectionAssignments(0)
            .split("-")
            .flatMap(_.toIntOption)
        val firstElfSections = firstElf.head to firstElf.last

        val secondElf =
          sectionAssignments(1)
            .split("-")
            .flatMap(_.toIntOption)
        val secondElfSections = secondElf.head to secondElf.last

        firstElfSections.containsSlice(secondElfSections) || secondElfSections.containsSlice(firstElfSections)
      }

  val partTwo =
    Source
      .fromFile("./input/day_4.txt")
      .getLines()
      .map(_.split(","))
      .count { sectionAssignments =>
        val firstElf =
          sectionAssignments(0)
            .split("-")
            .flatMap(_.toIntOption)
        val firstElfSections = firstElf.head to firstElf.last

        val secondElf =
          sectionAssignments(1)
            .split("-")
            .flatMap(_.toIntOption)
        val secondElfSections = secondElf.head to secondElf.last

        firstElfSections.contains(secondElfSections.min) || firstElfSections.contains(secondElfSections.max) ||
        secondElfSections.contains(firstElfSections.min) || secondElfSections.contains(firstElfSections.max)
      }

  println(s"Part 1: - $partOne")
  println(s"Part 2: - $partTwo")
}
