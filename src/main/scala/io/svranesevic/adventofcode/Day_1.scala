package io.svranesevic.adventofcode

import scala.io.Source
import scala.collection.mutable.PriorityQueue

object Day_1 extends App {

  var maxHeap = PriorityQueue.empty[Long]

  Source
    .fromFile("./1.txt")
    .getLines()
    .mkString("\n")
    .split("\n\n")
    .foreach { weights =>
      val elfWeight = weights.split('\n').flatMap(_.toLongOption).sum
      maxHeap.enqueue(elfWeight)
    }

  println("Top 3 elves:")
  println(maxHeap.dequeue())
  println(maxHeap.dequeue())
  println(maxHeap.dequeue())
}
