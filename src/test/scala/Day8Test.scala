import adventOfCode.Day8

class Day8Test extends org.scalatest.funsuite.AnyFunSuite {
  test("Day8 oneOption") {
    val digits = List(List('b','c'),List('a','b','c'))
    assert(Day8.oneOption(digits) === Option(List('b','c')))
  }

  test("Day8 fourOption") {
    val digits = List(List('b','c'),List('a','b','c'),List('f','b','c','g'))
    assert(Day8.fourOption(digits) === Option(List('f','b','c','g')))
  }

  test("Day8 sevenOption") {
    val digits = List(List('b','c'),List('a','b','c'),List('f','b','c','g'))
    assert(Day8.sevenOption(digits) === Option(List('a','b','c')))
  }

  test("Day8 eightOption") {
    val digits = List(List('b','c'),List('a','b','c','d','e','f','g'),List('f','b','c','g'))
    assert(Day8.eightOption(digits) === Option(List('a','b','c','d','e','f','g')))
  }

//   test("Day8 nineOption") {
//     val digits = List(List('b','c'),List('a','b','c','d','f','g'),List('f','b','c','g'))
//     assert(Day8.nineOption(digits) === Option(List('a','b','c','d','f','g')))
//   }
//   test("Day8 sixOption") {
//     val digits = List(List('b','c'),List('a','e','c','d','f','g'),List('f','b','c','g'))
//     assert(Day8.sixOption(digits) === Option(List('a','e','c','d','f','g')))
//   }

}
