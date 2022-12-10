package io.svranesevic.adventofcode

import scala.io.Source

object Day_8 extends App {

  val trees =
    Source
      .fromFile("./input/day_8.txt")
      .getLines()
      .map(_.split("").flatMap(_.toIntOption).toVector)
      .toVector

  val forest = Forest(trees)

  val partOne = forest.numVisibleTrees
  val partTwo = forest.highestScenicScore

  println(s"Part 1: $partOne")
  println(s"Part 2: $partTwo")
}

case class Forest(private val trees: Vector[Vector[Int]]) extends AnyVal {

  def highestScenicScore: Int = {
    val treesScenicScore =
      for {
        row <- 0 until trees.size
        col <- 0 until trees.map(_.size).max
      } yield scenicScore(row, col)

    treesScenicScore.maxOption.getOrElse(0)
  }

  private def scenicScore(row: Int, col: Int): Int =
    scenicScoreTowardNorth(row, col) * scenicScoreTowardSouth(row, col) *
      scenicScoreTowardWest(row, col) * scenicScoreTowardEast(row, col)

  private def scenicScoreTowardNorth(row: Int, col: Int): Int = {
    val rows = (0 until row).reverse
    val cols = col to col
    val heights = treeHeights(rows, cols)

    val height = treeHeight(row, col)

    val until = heights.takeWhile(_ < height).size

    val eq =
      if (heights.drop(until).nonEmpty) 1
      else 0

    until + eq
  }

  private def scenicScoreTowardSouth(row: Int, col: Int): Int = {
    val rows = row + 1 until trees.size
    val cols = col to col
    val heights = treeHeights(rows, cols)

    val height = treeHeight(row, col)

    val until = heights.takeWhile(_ < height).size

    val eq =
      if (heights.drop(until).nonEmpty) 1
      else 0

    until + eq
  }

  private def scenicScoreTowardWest(row: Int, col: Int): Int = {
    val rows = row to row
    val cols = (0 until col).reverse
    val heights = treeHeights(rows, cols)

    val height = treeHeight(row, col)

    val until = heights.takeWhile(_ < height).size

    val eq =
      if (heights.drop(until).nonEmpty) 1
      else 0

    until + eq
  }

  private def scenicScoreTowardEast(row: Int, col: Int): Int = {
    val rows = row to row
    val cols = col + 1 until trees.map(_.size).max
    val heights = treeHeights(rows, cols)

    val height = treeHeight(row, col)

    val until = heights.takeWhile(_ < height).size

    val eq =
      if (heights.drop(until).nonEmpty) 1
      else 0

    until + eq
  }

  def numVisibleTrees: Int = {
    val treesIsVisible =
      for {
        row <- 0 until trees.size
        col <- 0 until trees.map(_.size).max
      } yield isVisible(row, col)

    treesIsVisible.count(_ == true)
  }

  private def isVisible(row: Int, col: Int): Boolean =
    isVisibleFromNorth(row, col) || isVisibleFromSouth(row, col) ||
      isVisibleFromWest(row, col) || isVisibleFromEast(row, col)

  private def isVisibleFromNorth(row: Int, col: Int): Boolean = {
    val rows = 0 until row
    val cols = col to col
    val maxHeight = treeHeights(rows, cols).maxOption.getOrElse(-1)

    treeHeight(row, col) > maxHeight
  }

  private def isVisibleFromSouth(row: Int, col: Int): Boolean = {
    val rows = row + 1 until trees.size
    val cols = col to col
    val maxHeight = treeHeights(rows, cols).maxOption.getOrElse(-1)

    treeHeight(row, col) > maxHeight
  }

  private def isVisibleFromWest(row: Int, col: Int): Boolean = {
    val rows = row to row
    val cols = 0 until col
    val maxHeight = treeHeights(rows, cols).maxOption.getOrElse(-1)

    treeHeight(row, col) > maxHeight
  }

  private def isVisibleFromEast(row: Int, col: Int): Boolean = {
    val rows = row to row
    val cols = col + 1 until trees.map(_.size).max
    val maxHeight = treeHeights(rows, cols).maxOption.getOrElse(-1)

    treeHeight(row, col) > maxHeight
  }

  private def treeHeights(rows: Range, cols: Range): Vector[Int] = {
    val heights =
      for {
        row <- rows
        col <- cols
      } yield treeHeight(row, col)

    heights.toVector
  }

  private def treeHeight(row: Int, col: Int): Int =
    trees.lift(row).flatMap(_.lift(col)).getOrElse(-1)
}
