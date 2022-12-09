package adventOfCode

import scala.annotation.tailrec
import scala.io.Source
import scala.collection.mutable.ArraySeq

object Day6 extends App {

  val lines = Source.fromResource("day6Input.txt").getLines.toList
  val input = "\\d+".r.findAllIn(lines.head).map(_.toInt)

  val vector: collection.mutable.ArraySeq[Long] = ArraySeq.fill(9)(0)
  input.foreach(vector(_) += 1L)
  println(calculate((1 to 80).toList, vector).sum)
  println(calculate((1 to 256).toList, vector).sum)

  @tailrec
  def calculate(days: List[Int], population: ArraySeq[Long]): ArraySeq[Long] = {
    days match {
      case Nil => population
      case day :: days =>
        calculate(days, population.tail.updated(6, population.tail(6) + population.head) :+ population.head)
    }
  }
}
