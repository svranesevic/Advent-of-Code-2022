package io.svranesevic.adventofcode

import scala.io.Source

object Day_11 extends App {
  val monkeys =
    Source
      .fromFile("./input/day_11.txt")
      .getLines()
      .grouped(7)
      .map(Monkey.fromString)
      .toVector

  val partOne =
    play(monkeys, numRounds = 20, relief = _ / 3)
      .map(_.numInspections)
      .sorted(Ordering.Long.reverse)
      .take(2)
      .reduce(_ * _)

  val leastCommonMultipler = monkeys.map(_.testDivBy).product
  val partTwo =
    play(monkeys, numRounds = 10_000, relief = _ % leastCommonMultipler)
      .map(_.numInspections)
      .sorted(Ordering.Long.reverse)
      .take(2)
      .reduce(_ * _)

  println(s"Part 1: $partOne")
  println(s"Part 2: $partTwo")

  def play(monkeys: Vector[Monkey], numRounds: Int, relief: Long => Long): Vector[Monkey] = {
    val monkeysMap = monkeys.map { monkey => monkey.number -> monkey }.toMap
    (1 to numRounds)
      .foldLeft(monkeysMap) { case (monkeys, round) => playRound(monkeys, relief) }
      .map { case (_, monkey) => monkey }
      .toVector
  }

  def playRound(monkeys: Map[Int, Monkey], relief: Long => Long): Map[Int, Monkey] =
    monkeys.keys.toVector.sorted.foldLeft(monkeys) { case (monkeys, monkeyNumber) =>
      monkeys(monkeyNumber).playRound(monkeys, relief)
    }
}

case class Monkey(
    number: Int,
    items: Vector[Long],
    worryLevel: Long => Long,
    testDivBy: Int,
    toMonkeyIfTrue: Int,
    toMonkeyIfFalse: Int,
    numInspections: Long = 0
) {

  def playRound(monkeys: Map[Int, Monkey], relief: Long => Long): Map[Int, Monkey] =
    throwItems(monkeys, relief).updatedWith(number)(_.map(_.finishRound))

  private def throwItems(monkeys: Map[Int, Monkey], relief: Long => Long): Map[Int, Monkey] =
    items
      .foldLeft(monkeys) { case (monkeys, item) =>
        val inspectedItem = relief(worryLevel(item))
        val toMonkey =
          if (inspectedItem % testDivBy == 0) toMonkeyIfTrue
          else toMonkeyIfFalse

        monkeys.updatedWith(toMonkey)(_.map(_.catchItem(inspectedItem)))
      }

  private def finishRound: Monkey =
    copy(items = Vector.empty, numInspections = numInspections + items.length)

  private def catchItem(item: Long): Monkey =
    copy(items = this.items :+ item)
}

object Monkey {

  def fromString(monkeyStr: Seq[String]): Monkey = {
    // format: off
    val s"Monkey $number:"                        = monkeyStr(0)
    val s"  Starting items: $items"               = monkeyStr(1)
    val s"  Operation: new = $worryOp"            = monkeyStr(2)
    val s"  Test: divisible by $testDibBy"        = monkeyStr(3)
    val s"    If true: throw to monkey $ifTrue"   = monkeyStr(4)
    val s"    If false: throw to monkey $ifFalse" = monkeyStr(5)
    // format: on

    Monkey(
      number.toInt,
      items.split(", ").map(_.toLong).toVector,
      worryOpFromString(worryOp),
      testDibBy.toInt,
      ifTrue.toInt,
      ifFalse.toInt
    )
  }

  private def worryOpFromString(operationStr: String): Long => Long =
    operationStr match {
      case "old + old" => (old: Long) => old + old
      case "old * old" => (old: Long) => old * old
      case s"old + $x" => (old: Long) => old + x.toInt
      case s"old * $x" => (old: Long) => old * x.toInt
    }
}
