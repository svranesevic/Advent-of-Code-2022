package io.svranesevic.adventofcode

import scala.io.Source
import scala.collection.mutable.ArrayBuffer

object Day_7 extends App {

  val cd = """\$ cd ([\/ .. \w]+)""".r
  val dir = """dir (\w+)""".r
  val file = """(\d+) [. \w]+""".r

  val fs =
    Source
      .fromFile("./input/day_7.txt")
      .getLines()
      .foldLeft(Dir(name = "/")) {
        case (fs, cd("/"))    => fs.root
        case (fs, cd(".."))   => fs.cdParent
        case (fs, cd(into))   => fs.cd(into)
        case (fs, dir(name))  => fs.addDir(name)
        case (fs, file(size)) => fs.addFile(size.toLong)
        case (fs, _)          => fs
      }

  val partOne = fs.root.dirSizes.filter(_ <= 100_000).sum

  val fsSize = 70_000_000L
  val usedSpace = fs.root.size
  val unusedSpace = fsSize - usedSpace

  val updateSize = 30_000_000L
  val missingSpace = updateSize - unusedSpace
  val Some(partTwo) = fs.root.dirSizes.sorted.find(_ >= missingSpace)

  println(s"Part 1: $partOne")
  println(s"Part 2: $partTwo")
}

case class Dir(
    name: String,
    parent: Option[Dir] = None,
    dirs: ArrayBuffer[Dir] = ArrayBuffer.empty,
    fileSizes: ArrayBuffer[Long] = ArrayBuffer.empty
) {

  def addDir(name: String): Dir = {
    dirs.addOne(Dir(name, parent = Some(this)))
    this
  }

  def addFile(size: Long): Dir = {
    fileSizes.addOne(size)
    this
  }

  def cd(into: String): Dir =
    dirs.find(_.name == into).get

  def cdParent: Dir =
    parent match {
      case Some(parent) => parent
      case None         => this
    }

  def root: Dir =
    parent match {
      case Some(parent) => parent.root
      case None         => this
    }

  def dirSizes: List[Long] =
    this.size +: dirs.toList.flatMap(_.dirSizes)

  def size: Long =
    fileSizes.sum + dirs.map(_.size).sum
}
