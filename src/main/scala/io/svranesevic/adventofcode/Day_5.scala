package io.svranesevic.adventofcode

import scala.io.Source

object Day_5 extends App {

  val stacks =
    Stacks.from(
      Map(
        1 -> "GTRW",
        2 -> "GCHPMSVW",
        3 -> "CLTSGM",
        4 -> "JHDMWRF",
        5 -> "PQLHSWFJ",
        6 -> "PJDNFMS",
        7 -> "ZBDFGCSJ",
        8 -> "RTB",
        9 -> "HNWLC"
      )
    )

  val partOne =
    Source
      .fromFile("./input/day_5.txt")
      .getLines()
      .flatMap(MoveAction.fromString)
      .foldLeft(stacks) { case (stacks, action) =>
        CrateMover9000.move(stacks, action)
      }
      .topline

  val partTwo =
    Source
      .fromFile("./input/day_5.txt")
      .getLines()
      .flatMap(MoveAction.fromString)
      .foldLeft(stacks) { case (stacks, action) =>
        CrateMover9001.move(stacks, action)
      }
      .topline

  println(s"Part 1: $partOne")
  println(s"Part 2: $partTwo")
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

  def from(map: Map[Int, String]): Stacks = {
    val stacks =
      map.map { case (stack, crates) => stack -> crates.toVector }
    Stacks(stacks)
  }
}

case class MoveAction(from: Int, to: Int, numCrates: Int)
object MoveAction {

  def fromString(moveActionStr: String): Option[MoveAction] = {
    moveActionStr match {
      case s"move $numCratesStr from $fromStr to $toStr" =>
        for {
          numCrates <- numCratesStr.toIntOption
          from <- fromStr.toIntOption
          to <- toStr.toIntOption
        } yield MoveAction(from, to, numCrates)

      case _ => None
    }
  }
}
