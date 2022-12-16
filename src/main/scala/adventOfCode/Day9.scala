package adventOfCode

import scala.io.Source

object Day9 extends App {

    val heightMap = Source.fromResource("day9Input.txt").getLines.toList.map(line => line.toList.map(c => Integer.parseInt(c.toString())))

    val points = for {
        y <- 0 until heightMap.size
        x <- 0 until heightMap.head.size
    } yield Point(y, x)

    val lowestPoints = points.filter(p => isLowPoint(p))
    val part1 = lowestPoints.map(_.value + 1).sum
    println(part1)

    def exploreBasin(visit: Set[Point], seen: Set[Point]): Set[Point] = {
    if (visit.nonEmpty) {
      val next = visit.flatMap(_.getAdjacentPoints.filter(_.value < 9).filterNot(seen.contains))
      exploreBasin(next, seen ++ next)
    }
    else
      seen
    }

    val basins = lowestPoints.map(point => exploreBasin(Set(point), Set(point)).size)
    println(basins.sortBy(-_).take(3).product)

    def isLowPoint(point: Point): Boolean = point.getAdjacentPoints.forall(p => p.hasHigherValue(point.value))

    case class Point(y: Int, x: Int) {
        def exists: Boolean = heightMap.lift(y).flatMap(_.lift(x)).nonEmpty
        def value: Int = heightMap(y)(x)
        def hasHigherValue(valueIn: Int): Boolean = value > valueIn
        def getAdjacentPoints: Set[Point] =
            Set(Point(y + 1, x ), Point(y, x + 1), Point(y - 1, x), Point(y, x - 1)).filter(_.exists)
    }

}

