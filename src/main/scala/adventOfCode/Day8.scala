package adventOfCode

import scala.io.Source

object Day8 extends App {

    val lines = Source.fromResource("day8Input.txt").getLines.toList
    val entries = lines map { line => line.splitAt(line.indexOf("|")) match {
        case (signalPattern, outputValues) => Entry(signalPattern, outputValues)}
    }

    val part1 = entries.map {entry => entry.values.foldLeft(0){ (sum, digit) =>        
        digit match {
        case digit if List(2,3,4,7).contains(digit.size) => sum + 1
        case _ => sum
    }}}.sum

    println(part1)

    val decodedPatternsPlusValues = entries.map { entry => Tuple2((identifyDigits(entry.patterns)), entry.values )}
    val decodedValues = decodedPatternsPlusValues.map{
        case (decodedPatterns, digits) =>
            digits.map { digit => decodedPatterns.collectFirst{
                case digitObj: Digit if digitObj.is(digit) => digitObj.value}.get }
    }
    val numbers = decodedValues.map{value(_)}

    println(numbers.sum)

    def identifyDigits(digits: List[List[Char]]): List[Digit] = {
        List(identify0(digits),
        identify1(digits),
        identify2(digits),
        identify3(digits),
        identify4(digits),
        identify5(digits),
        identify6(digits),
        identify7(digits),
        identify8(digits),
        identify9(digits))
    }

    def identify1(digits: List[List[Char]]): Digit = digits.collectFirst{case chars: List[Char] if chars.size == 2 => Digit(1, chars)}.get
    def identify4(digits: List[List[Char]]): Digit = digits.collectFirst{case chars: List[Char] if chars.size == 4 => Digit(4, chars)}.get
    def identify7(digits: List[List[Char]]): Digit = digits.collectFirst{case chars: List[Char] if chars.size == 3 => Digit(7, chars)}.get
    def identify8(digits: List[List[Char]]): Digit = digits.collectFirst{case chars: List[Char] if chars.size == 7 => Digit(8, chars)}.get
    
    def identify0(digits: List[List[Char]]): Digit = digits.collectFirst{ case zeroCandidate: List[Char] if
        zeroCandidate.size == 6 &&
        identify7(digits).overlaps(zeroCandidate) &&
        identify9(digits).isNot(zeroCandidate) =>
            Digit(0,zeroCandidate)}.get

    def identify9(digits: List[List[Char]]): Digit = digits.collectFirst{ case nineCandidate: List[Char] if
        nineCandidate.size == 6 &&
        identify4(digits).overlaps(nineCandidate) =>
            Digit(9,nineCandidate)}.get

    def identify6(digits: List[List[Char]]): Digit = digits.collectFirst{ case sixCandidate: List[Char] if
        sixCandidate.size == 6 && identify0(digits).isNot(sixCandidate) &&
        identify9(digits).isNot(sixCandidate) => Digit(6, sixCandidate)}.get

    def identify3(digits: List[List[Char]]): Digit = digits.collectFirst { case threeCandidate: List[Char] if
        threeCandidate.size == 5 &
        identify1(digits).overlaps(threeCandidate) => 
            Digit(3, threeCandidate)}.get

    def identify5(digits: List[List[Char]]): Digit = digits.collectFirst { case fiveCandidate if
        fiveCandidate.size == 5 && 
        identify6(digits).overlappedBy(fiveCandidate) => Digit(5, fiveCandidate)}.get

    def identify2(digits: List[List[Char]]): Digit = digits.collectFirst { case twoCandidate: List[Char] if
        twoCandidate.size == 5 && 
        identify3(digits).isNot(twoCandidate) &&
        identify5(digits).isNot(twoCandidate)=> Digit(2, twoCandidate)}.get


    def value(numbers: List[Int]): Int = {
        numbers.zip(List(1000,100,10,1)).map{case (number, weight) => number * weight}.sum
    }
}

case class Entry(signalPattern: String, outputValues: String) {
        def patterns: List[List[Char]] = "[a-g]+".r.findAllIn(signalPattern).map(_.toList).toList
        def values: List[List[Char]] = "[a-g]+".r.findAllIn(outputValues).map(_.toList).toList
}

case class Digit(value: Int, segments: List[Char]) {
    def overlaps(otherDigit: List[Char]): Boolean = {
        // True if all segments of this digit are also in otherDigit
        segments.forall(otherDigit.contains(_))
    }
    def overlappedBy(otherDigit: List[Char]): Boolean = {
        // True if all segments of otherDigit are also in this digit
        otherDigit.forall(segments.contains(_))
    }
    def isNot(otherDigit: List[Char]): Boolean = {
        // True if one of the segments is not in otherDigit
        segments.sorted != otherDigit.sorted   }

    def is(digit: List[Char]): Boolean = {
        segments.sorted == digit.sorted }
}