package io.svranesevic.adventofcode

import scala.io.Source

object Day_2 extends App {
  val score_1 =
    Source
      .fromFile("./2.txt")
      .getLines()
      .map(_.split(" ").flatMap(Shape.fromString))
      .foldLeft(0L) { case (score, shapes) =>
        val opponent = shapes(0)
        val me = shapes(1)
        score + me.score(opponent)
      }

  val score_2 =
    Source
      .fromFile("./2.txt")
      .getLines()
      .flatMap { line =>
        val opponentAndOutcome = line.split(" ")
        for {
          opponent <- Shape.fromString(opponentAndOutcome(0))
          outcome <- Outcome.fromString(opponentAndOutcome(1))
        } yield opponent -> outcome
      }
      .foldLeft(0L) { case (score, (opponent, outcome)) =>
        val me =
          outcome match {
            case Outcome.Win  => opponent.losesAgainst
            case Outcome.Lose => opponent.winsAgainst
            case Outcome.Draw => opponent.evenAgainst
          }

        score + me.score(opponent)
      }

  println(s"Score 1: $score_1")
  println(s"Score 2: $score_2")
}

sealed trait Outcome
object Outcome {

  case object Win extends Outcome
  case object Lose extends Outcome
  case object Draw extends Outcome

  def fromString(str: String): Option[Outcome] =
    str match {
      case "X" => Some(Outcome.Lose)
      case "Y" => Some(Outcome.Draw)
      case "Z" => Some(Outcome.Win)
      case _   => None
    }
}

sealed trait Shape {

  val winsAgainst: Shape =
    this match {
      case Shape.Rock     => Shape.Scissors
      case Shape.Paper    => Shape.Rock
      case Shape.Scissors => Shape.Paper
    }

  val losesAgainst: Shape =
    this match {
      case Shape.Rock     => Shape.Paper
      case Shape.Paper    => Shape.Scissors
      case Shape.Scissors => Shape.Rock
    }

  val evenAgainst: Shape = this

  def score(against: Shape): Int = outcomeScore(against) + shapeScore

  private def outcomeScore(against: Shape): Int =
    if (this.winsAgainst == against) 6
    else if (this.evenAgainst == against) 3
    else 0

  private val shapeScore: Int =
    this match {
      case Shape.Rock     => 1
      case Shape.Paper    => 2
      case Shape.Scissors => 3
    }
}
object Shape {

  case object Rock extends Shape
  case object Paper extends Shape
  case object Scissors extends Shape

  def fromString(str: String): Option[Shape] =
    str match {
      case "A" | "X" => Some(Shape.Rock)
      case "B" | "Y" => Some(Shape.Paper)
      case "C" | "Z" => Some(Shape.Scissors)
      case _         => None
    }
}
