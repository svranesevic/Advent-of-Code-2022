package io.svranesevic.adventofcode

import scala.io.Source

object Day_13 extends App {

  val partOne =
    Source
      .fromFile("./input/day_13.txt")
      .mkString
      .split("\n\n")
      .toList
      .zipWithIndex
      .map { case (packets, index) =>
        val Array(left, right) = packets.split("\n")
        (parse(left), parse(right)) -> index
      }
      .flatMap { case ((left, right), index) =>
        Option.when(left.inOrder(right).getOrElse(false))(index + 1)
      }
      .sum

  val dividerPackets = "[[2]]" :: "[[6]]" :: Nil
  val orderedPackets =
    Source
      .fromFile("./input/day_13.txt")
      .getLines()
      .filterNot(_.isBlank())
      .toList
      .appendedAll(dividerPackets)
      .map(parse)
      .sortWith((l, r) => l.inOrder(r).getOrElse(false))

  val partTwo = {
    val a = orderedPackets.indexOf(parse("[[2]]")) + 1
    val b = orderedPackets.indexOf(parse("[[6]]")) + 1
    a * b
  }

  println(s"Part 1: $partOne")
  println(s"Part 2: $partTwo")

  assert(partOne == 5588)
  assert(partTwo == 23958)

  sealed trait Packet {
    def inOrder(that: Packet): Option[Boolean] =
      (this, that) match {
        case (Integer(left), Integer(right))   => intsInOrder(left, right)
        case (left: Integer, Multiple(right))  => listsInOrder(left :: Nil, right)
        case (Multiple(left), right: Integer)  => listsInOrder(left, right :: Nil)
        case (Multiple(left), Multiple(right)) => listsInOrder(left, right)
      }

    def intsInOrder(left: Int, right: Int): Option[Boolean] =
      Option.when(left != right)(left < right)

    def listsInOrder(left: List[Packet], right: List[Packet]): Option[Boolean] =
      (left.headOption, right.headOption) match {
        case (Some(l), Some(r)) => l.inOrder(r).orElse(listsInOrder(left.tail, right.tail))
        case (None, None)       => None
        case (_, None)          => Some(false)
        case (None, _)          => Some(true)
      }
  }
  final case class Integer(value: Int) extends Packet
  final case class Multiple(values: List[Packet]) extends Packet

  def parse(input: String): Packet = {
    import cats.parse.{Parser, Parser0}
    import cats.parse.Parser.{char, defer}
    import cats.parse.Numbers.digits

    def element: Parser[Packet] = one | defer(many)
    def one: Parser[Integer] = digits.map(_.toInt).map(Integer)
    def many: Parser[Multiple] = (char('[') *> element.repSep0(char(',')) <* char(']')).map(Multiple)

    element.parseAll(input).toOption.get
  }
}
