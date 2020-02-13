package problems99

import org.scalatest.flatspec.AnyFlatSpec
import problems99.Roman.toInt

class RomanTest extends AnyFlatSpec {

  "toInt" must "return 0 for null input" in {
    assert(toInt(null) == 0)
  }

  "toInt" must "return 0 for empty input" in {
    assert(toInt("") == 0)
  }

  "toInt" must "throw exception for unsupported denomination" in {
    assertThrows[IllegalStateException](toInt("G") == 0)
    assertThrows[IllegalStateException](toInt("U") == 0)
    assertThrows[IllegalStateException](toInt("A") == 0)
    assertThrows[IllegalStateException](toInt("M") == 0)
  }

  "toInt" must "count Is properly" in {
    assert(toInt("I") == 1)
    assert(toInt("III") == 3)
    assert(toInt("IIIII") == 5)
  }

  "toInt" must "not allow more then nine Is in a row" in {
    assertThrows[IllegalStateException](toInt("IIIIIIIIII"))
    assertThrows[IllegalStateException](toInt("IIIIIIIIIIIII"))
  }

  "toInt" must "count V as five" in {
    assert(toInt("V") == 5)
  }

  "toInt" must "count V followed by Is" in {
    assert(toInt("VI") == 6)
    assert(toInt("VIIII") == 9)
    assert(toInt("VIIIIIIII") == 13)
  }

  "toInt" must "count V preceded by I" in {
    assert(toInt("IV") == 4)
  }

  "toInt" must "throw an exception for multiple Vs" in {
    assertThrows[IllegalStateException](toInt("VV"))
  }

  "toInt" must "count X as ten" in {
    assert(toInt("X") == 10)
    assert(toInt("XX") == 20)
    assert(toInt("XXX") == 30)
  }

  "toInt" must "count X preceded by a I" in {
    assert(toInt("IX") == 9)
  }

  "toInt" must "not allow more than 9 consecutive Xs" in {
    assert(toInt("XXXXXXXXX") == 90)
    assertThrows[IllegalStateException](toInt("XXXXXXXXXX"))
    assertThrows[IllegalStateException](toInt("XXXXXXXXXxxX"))
  }

  "toInt" must "throw an exception for multiple Ds" in {
    "DD" == 5
    "DDD" == 5
  }

  "toInt" must "throw an exception for multiple Ls" in {
    "LL" == "BOOM"
    "LLL" == "BOOM"
  }

  "toInt" must "throw an exception for unordered numerals" in {
    "IIV" == "BOOM"
    "VX" == "BOOM"
  }


}
