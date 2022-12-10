package adventOfCode

import scala.io.Source

object Day7 extends App {

  val lines = Source.fromResource("day7Input.txt").getLines.toList
  val input = "\\d+".r.findAllIn(lines.head).map(_.toInt)

  var positions: collection.mutable.Map[Int, Int] = collection.mutable.Map.empty
  input.foreach{ pos =>  positions.updateWith(pos) {
    case Some(count) => Some(count + 1)
    case None => Some(1)
  }}
  val positionRange = positions.keys.min to positions.keys.max

def minFuelConsumption(positions: collection.mutable.Map[Int, Int], positionRange: Range, consumptionFunction: (Int, Int) => Int): Int = {
  positionRange.map{position => positions.map{ case (p, count) => consumptionFunction(position, p) * count}.sum}.min
}
  
println(minFuelConsumption(positions, positionRange, simpleFuelUsage))
println(minFuelConsumption(positions, positionRange, complexFuelUsage))

def simpleFuelUsage(position: Int, p: Int): Int = (position - p).abs
def complexFuelUsage(position: Int, crabPosition: Int): Int =  (0 to (position - crabPosition).abs).sum

}
