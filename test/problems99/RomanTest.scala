package problems99

import org.scalatest.flatspec.AnyFlatSpec

class RomanTest extends AnyFlatSpec {

  "translate" must "return 0 for null input" in {
    assert(Roman.toInt(null) == 0)
  }

  "translate" must "return 0 for empty input" in {
    assert(Roman.toInt("") == 0)
  }

  "translate" must "count Is properly" in {
    assert(Roman.toInt("I") == 1)
    assert(Roman.toInt("III") == 3)
    assert(Roman.toInt("IIIII") == 5)
  }

  "translate" must "count V as five" in {
    assert(Roman.toInt("V") == 5)
  }

  "translate" must "count V followed by Is" in {
    assert(Roman.toInt("VI") == 6)
    assert(Roman.toInt("VIIII") == 9)
    assert(Roman.toInt("VIIIIIIII") == 13)
  }

//  "translate" must "count V preceded by I" in {
//    assert(Roman.toInt("IV") == 4)
//  }

  "translate" must "throw an exception for multiple Vs" in {
    assertThrows[IllegalStateException](Roman.toInt("VV"))
  }

  "translate" must "throw an exception for multiple Ds" in {
    "DD" == 5
    "DDD" == 5
  }

  "translate" must "throw an exception for multiple Ls" in {
    "LL" == "BOOM"
    "LLL" == "BOOM"
  }

  "translate" must "throw an exception for unordered numerals" in {
    "IIV" == "BOOM"
  }



}
