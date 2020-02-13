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
    assertThrows[IllegalStateException](toInt("VIV"))
  }

  "toInt" must "count X as ten" in {
    assert(toInt("X") == 10)
    assert(toInt("XX") == 20)
    assert(toInt("XXX") == 30)
  }

  "toInt" must "count X preceded by a I" in {
    assert(toInt("IX") == 9)
  }

  "toInt" must "count X followed by Is" in {
    assert(toInt("XI") == 11)
    assert(toInt("XIIII") == 14)
    assert(toInt("XIIIIIIII") == 18)
  }

  "toInt" must "count X followed by Vs and Is" in {
    assert(toInt("XVI") == 16)
    assert(toInt("XIV") == 14)
    assert(toInt("XVIII") == 18)
  }


  "toInt" must "not allow more than 9 consecutive Xs" in {
    assert(toInt("XXXXXXXXX") == 90)
    assertThrows[IllegalStateException](toInt("XXXXXXXXXX"))
    assertThrows[IllegalStateException](toInt("XXXXXXXXXXXX"))
  }

  "toInt" must "count L as fifty" in {
    assert(toInt("L") == 50)
  }

  "toInt" must "throw an exception for multiple Ls" in {
    assertThrows[IllegalStateException](toInt("LL"))
    assertThrows[IllegalStateException](toInt("LIL"))
  }

  "toInt" must "count Ls followed by Is" in {
    assert(toInt("LI") == 51)
    assert(toInt("LIIII") == 54)
    assert(toInt("LIIIIIIII") == 58)
  }

  "toInt" must "count L followed by Vs and Is" in {
    assert(toInt("LVI") == 56)
    assert(toInt("LIV") == 54)
    assert(toInt("LVIII") == 58)
  }

  "toInt" must "count L followed by Xs, Vs and Is" in {
    assert(toInt("LXVI") == 66)
    assert(toInt("LXXIV") == 74)
    assert(toInt("LXXVIII") == 78)
  }

  "toInt" must "count L preceded by a smaller numeral by Xs, Vs and Is" in {
    assert(toInt("IL") == 49)
    assert(toInt("VL") == 45)
    assert(toInt("XL") == 40)
  }

  "toInt" must "count C as one hundred" in {
    assert(toInt("C") == 100)
    assert(toInt("CC") == 200)
    assert(toInt("CCCCC") == 500)
  }

  "toInt" must "not allow more than 9 consecutive Cs" in {
    assert(toInt("CCCCCCCCC") == 900)
    assertThrows[IllegalStateException](toInt("CCCCCCCCCC"))
    assertThrows[IllegalStateException](toInt("CCCCCCCCCCCC"))
  }

  "toInt" must "count C followed by Is" in {
    assert(toInt("CI") == 101)
    assert(toInt("CIIII") == 104)
    assert(toInt("CIIIIIIII") == 108)
  }

  "toInt" must "count C followed by Vs and Is" in {
    assert(toInt("CVI") == 106)
    assert(toInt("CIV") == 104)
    assert(toInt("CVIII") == 108)
  }

  "toInt" must "count C followed by Xs, Vs and Is" in {
    assert(toInt("CXVI") == 116)
    assert(toInt("CXXIV") == 124)
    assert(toInt("CXXVIII") == 128)
  }

  "toInt" must "count C followed by Ls, Xs, Vs and Is" in {
    assert(toInt("CLXVI") == 166)
    assert(toInt("CLXXIV") == 174)
    assert(toInt("CLXXVIII") == 178)
    assert(toInt("CCLXXVIII") == 278)
  }






  "toInt" must "throw an exception for unordered numerals" in {
    "IIV" == "BOOM"
    "VX" == "BOOM"
    "XXL" == "BOOM"
    "XLC" == "BOOM"
  }


}
