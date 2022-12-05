package io.svranesevic.adventofcode

import scala.io.Source

object Day_5 extends App {

  val stacks =
    Stacks.from(
      Map(
        1 -> Vector('G', 'T', 'R', 'W'),
        2 -> Vector('G', 'C', 'H', 'P', 'M', 'S', 'V', 'W'),
        3 -> Vector('C', 'L', 'T', 'S', 'G', 'M'),
        4 -> Vector('J', 'H', 'D', 'M', 'W', 'R', 'F'),
        5 -> Vector('P', 'Q', 'L', 'H', 'S', 'W', 'F', 'J'),
        6 -> Vector('P', 'J', 'D', 'N', 'F', 'M', 'S'),
        7 -> Vector('Z', 'B', 'D', 'F', 'G', 'C', 'S', 'J'),
        8 -> Vector('R', 'T', 'B'),
        9 -> Vector('H', 'N', 'W', 'L', 'C')
      )
    )

  val partOne =
    Source
      .fromFile("./5.txt")
      .getLines()
      .flatMap(MoveAction.fromString)
      .foldLeft(stacks) { case (stacks, action) =>
        CrateMover9000.move(stacks, action)
      }
      .topline

  val partTwo =
    Source
      .fromFile("./5.txt")
      .getLines()
      .flatMap(MoveAction.fromString)
      .foldLeft(stacks) { case (stacks, action) =>
        CrateMover9001.move(stacks, action)
      }
      .topline

  println(s"Part one: $partOne")
  println(s"Part two: $partTwo")
}

final case class Stacks(private val under: Map[Int, Vector[Char]]) extends AnyVal {

  def topline: String =
    under.toList
      .sortBy { case (stack, _) => stack }
      .flatMap { case (_, crates) => crates.lastOption }
      .mkString

  def getCrates(stackNumber: Int): Vector[Char] = under.getOrElse(stackNumber, Vector.empty)

  def addCrates(stackNumber: Int, crates: Vector[Char]): Stacks = {
    val current = getCrates(stackNumber)
    copy(under.updated(stackNumber, current ++ crates))
  }

  def removeCrates(stackNumber: Int, numCrates: Int): Stacks =
    copy(under.updatedWith(stackNumber)(_.map(_.dropRight(numCrates))))
}

object CrateMover9000 {
  def move(stacks: Stacks, action: MoveAction): Stacks =
    move(stacks, action.from, action.to, action.numCrates)

  private def move(stacks: Stacks, fromStack: Int, toStack: Int, numCrates: Int): Stacks = {
    val from = stacks.getCrates(fromStack)
    val crates = from.takeRight(numCrates).reverse

    stacks
      .removeCrates(fromStack, numCrates)
      .addCrates(toStack, crates)
  }
}

object CrateMover9001 {
  def move(stacks: Stacks, action: MoveAction): Stacks =
    move(stacks, action.from, action.to, action.numCrates)

  private def move(stacks: Stacks, fromStack: Int, toStack: Int, numCrates: Int): Stacks = {
    val from = stacks.getCrates(fromStack)
    val crates = from.takeRight(numCrates)

    stacks
      .removeCrates(fromStack, numCrates)
      .addCrates(toStack, crates)
  }
}

object Stacks {

  def from(map: Map[Int, Vector[Char]]) = Stacks(map)
}

case class MoveAction(from: Int, to: Int, numCrates: Int)
object MoveAction {
  def fromString(moveActionStr: String): Option[MoveAction] = {
    val split = moveActionStr.split(" ").toList
    for {
      numCrates <- split.drop(1).headOption.flatMap(_.toIntOption)
      from <- split.drop(3).headOption.flatMap(_.toIntOption)
      to <- split.drop(5).headOption.flatMap(_.toIntOption)
    } yield MoveAction(from, to, numCrates)
  }
}
