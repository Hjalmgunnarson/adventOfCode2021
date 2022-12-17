package adventOfCode

import scala.io.Source

object Day10 extends App {

    val lines = Source.fromResource("day10Input.txt").getLines.toList
    val pairs: Map[Char, Char] = Map('}' -> '{', ']' -> '[', ')' -> '(', '>' -> '<' )
    val reversePairs = for ((k,v) <- pairs) yield (v, k)
    val openers = pairs.values.toSet
    val closers = pairs.keys
    val completeValues: Map[Char, Int] = Map('}' -> 3, ']' -> 2, ')' -> 1, '>' -> 4 )

    val part1 = lines.map(parseLine(_)).collect{ case c: InvalidChar => c.errorValue}
    println(part1.sum)

    val missingSymbols = lines.map(parseLine(_)).collect{ case line: IncompleteLine => {
      line.stack.map(c => completeValues(reversePairs(c))).toList
    }}
    
    val part2 = missingSymbols.map{ line => line.foldLeft(0L){ (total, i) => (total * 5) + i }}.sorted
    println(part2(part2.size / 2))

    def parseLine(line: String): Result = {
      val stack: collection.mutable.Stack[Char] = collection.mutable.Stack.empty
        line.toList.foreach{token => token match {
          case token if openers.contains(token) => stack.push(token)
          case token if stack.top == pairs(token) => stack.pop
          case _ => return InvalidChar(token)
        }}
        IncompleteLine(stack)
    }

    trait Result
    case class InvalidChar(c: Char) extends Result{
      def errorValue: Int = Map('}' -> 1197, ']' -> 57, ')' -> 3, '>' -> 25137 )(c)
    }

    case class IncompleteLine(stack: collection.mutable.Stack[Char]) extends Result {

    }
}

