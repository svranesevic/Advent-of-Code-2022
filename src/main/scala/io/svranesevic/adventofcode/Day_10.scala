package io.svranesevic.adventofcode

import scala.io.Source

object Day_10 extends App {
  val cpu =
    Source
      .fromFile("./input/day_10.txt")
      .getLines()
      .foldLeft(Cpu()) {
        case (cpu, s"addx $x") => cpu.addX(x.toInt)
        case (cpu, "noop")     => cpu.noop
        case (cpu, _)          => cpu
      }

  def sumOfSignalStrengths(cpu: Cpu): Int = {
    val cycles = List(20, 60, 100, 140, 180, 220)
    cycles.map { cycle =>
      val x = cpu.xDuringCycle(cycle)
      cycle * x
    }.sum
  }

  val partOne = sumOfSignalStrengths(cpu)

  val screenWidth = 40
  val screenHeight = 6
  val screenSize = screenWidth * screenHeight
  val partTwo =
    (0 until screenSize)
      .map { cycle =>
        val rayX = cycle % screenWidth
        val spriteX = cpu.xAtEndOfCycle(cycle)

        if ((rayX - spriteX).abs <= 1) "#"
        else "."
      }
      .grouped(screenWidth)
      .map(_.mkString)
      .mkString("\n")

  println(s"Part 1: $partOne")
  println(s"Part 2:\n$partTwo")
}

case class Cpu(private val cycle: Int = 1, private val x: Int = 1, private val history: Map[Int, Int] = Map.empty) {

  def addX(n: Int): Cpu =
    tick.tick.copy(x = x + n)

  def noop: Cpu = tick

  private def tick: Cpu =
    copy(
      cycle = cycle + 1,
      history = history + (cycle -> x)
    )

  def xAtEndOfCycle(n: Int): Int =
    xDuringCycle(n + 1)

  def xDuringCycle(n: Int): Int =
    if (n >= cycle) x
    else history.getOrElse(n, 1)
}
