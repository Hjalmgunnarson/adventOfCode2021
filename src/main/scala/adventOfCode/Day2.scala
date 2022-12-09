package adventOfCode

import scala.annotation.tailrec
import scala.io.Source

object Day2 extends App {
  val test = List("forward 5", "down 5", "forward 8", "up 3", "down 8", "forward 2")

  val commands = Source.fromResource("day2Input.txt").getLines.toList
  val position = move(commands, Tuple2(0, 0))
  println("Part 1 :" + position._1 * position._2)
  println("Part 2 :" + move(commands, PositionAndAim(0, 0, 0)).answer)

  // Part 1
  @tailrec
  def move(commands: List[String], position: (Int, Int)): (Int, Int) = {
    commands match {
      case Nil => position
      case command :: tail =>
        val movement = processCommand(command)
        move(tail, Tuple2(position._1 + movement._1, position._2 + movement._2))
    }
  }

  // Part 2
  @tailrec
  def move(commands: List[String], positionAndAim: PositionAndAim): PositionAndAim = {
    commands match {
      case Nil => positionAndAim
      case command :: tail =>
        val movement = processCommand(command)
        move(tail, PositionAndAim(
          positionAndAim.x + movement._1,
          positionAndAim.y + positionAndAim.aim * movement._1,
          positionAndAim.aim + movement._2)
        )
    }
  }

  def processCommand(command: String): (Int, Int) = {
    val regex = "([\\D]+) ([\\d]+)".r
    val regex(c, i) = command
    c match {
      case "forward" => Tuple2(i.toInt, 0)
      case "up" => Tuple2(0, 0 - i.toInt)
      case "down" => Tuple2(0, i.toInt)
    }
  }

  case class PositionAndAim(x: Int, y: Int, aim: Int) {
    override def toString: String = x + " " + y + " " + aim

    def answer: Int = x * y
  }
}
