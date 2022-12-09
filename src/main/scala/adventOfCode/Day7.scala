package adventOfCode

import scala.io.Source

object Day7 extends App {

  val lines = Source.fromResource("day7Input.txt").getLines.toList
  val input = "\\d+".r.findAllIn(lines.head).map(_.toInt)

  var states: collection.mutable.Map[Int, Int] = collection.mutable.Map.empty
  
println("Stop")

}
