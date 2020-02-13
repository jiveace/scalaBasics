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
    assertThrows[IllegalStateException](toInt("P") == 0)
    assertThrows[IllegalStateException](toInt("A") == 0)
    assertThrows[IllegalStateException](toInt("R") == 0)
    assertThrows[IllegalStateException](toInt("T") == 0)
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

  "toInt" must "count L preceded by a smaller numeral" in {
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

  "toInt" must "count C preceded by a smaller numeral" in {
    assert(toInt("IC") == 99)
    assert(toInt("VC") == 95)
    assert(toInt("XC") == 90)
    assert(toInt("LC") == 50)
  }

  "toInt" must "count D as five hundred" in {
    assert(toInt("D") == 500)
  }

  "toInt" must "throw an exception for multiple Ds" in {
    assertThrows[IllegalStateException](toInt("DD"))
    assertThrows[IllegalStateException](toInt("DID"))
  }


  "toInt" must "count D followed by Is" in {
    assert(toInt("DI") == 501)
    assert(toInt("DIIII") == 504)
    assert(toInt("DIIIIIIII") == 508)
  }

  "toInt" must "count D followed by Vs and Is" in {
    assert(toInt("DVI") == 506)
    assert(toInt("DIV") == 504)
    assert(toInt("DVIII") == 508)
  }

  "toInt" must "count D followed by Xs, Vs and Is" in {
    assert(toInt("DXVI") == 516)
    assert(toInt("DXXIV") == 524)
    assert(toInt("DXXVIII") == 528)
  }

  "toInt" must "count D followed by Ls, Xs, Vs and Is" in {
    assert(toInt("DLXVI") == 566)
    assert(toInt("DLXXIV") == 574)
    assert(toInt("DLXXVIII") == 578)
  }


  "toInt" must "count D followed by Cs, Ls, Xs, Vs and Is" in {
    assert(toInt("DCLXVI") == 666)
    assert(toInt("DCLXXIV") == 674)
    assert(toInt("DCLXXVIII") == 678)
    assert(toInt("DCCLXXVIII") == 778)
  }

  "toInt" must "count D preceded by a smaller numeral" in {
    assert(toInt("ID") == 499)
    assert(toInt("VD") == 495)
    assert(toInt("XD") == 490)
    assert(toInt("LD") == 450)
    assert(toInt("CD") == 400)
  }

  "toInt" must "count M as one thousand" in {
    assert(toInt("M") == 1000)
  }

  "toInt" must "count M followed by Is" in {
    assert(toInt("MI") == 1001)
    assert(toInt("MIIII") == 1004)
    assert(toInt("MIIIIIIII") == 1008)
  }

  "toInt" must "count M followed by Vs and Is" in {
    assert(toInt("MVI") == 1006)
    assert(toInt("MIV") == 1004)
    assert(toInt("MVIII") == 1008)
  }

  "toInt" must "count M followed by Xs, Vs and Is" in {
    assert(toInt("MXVI") == 1016)
    assert(toInt("MXXIV") == 1024)
    assert(toInt("MXXVIII") == 1028)
  }

  "toInt" must "count M followed by Ls, Xs, Vs and Is" in {
    assert(toInt("MLXVI") == 1066)
    assert(toInt("MLXXIV") == 1074)
    assert(toInt("MLXXVIII") == 1078)
  }

  "toInt" must "count M followed by Cs, Ls, Xs, Vs and Is" in {
    assert(toInt("MCLXVI") == 1166)
    assert(toInt("MCLXXIV") == 1174)
    assert(toInt("MCLXXVIII") == 1178)
    assert(toInt("MCCLXXVIII") == 1278)
  }

  "toInt" must "count M followed by D's, Cs, Ls, Xs, Vs and Is" in {
    assert(toInt("MDCLXVI") == 1666)
    assert(toInt("MDCLXXIV") == 1674)
    assert(toInt("MDCLXXVIII") == 1678)
    assert(toInt("MMMDCCLXXVIII") == 3778)
  }

  "toInt" must "count M preceded by a smaller numeral" in {
    assert(toInt("IM") == 999)
    assert(toInt("VM") == 995)
    assert(toInt("XM") == 990)
    assert(toInt("LM") == 950)
    assert(toInt("CM") == 900)
    assert(toInt("DM") == 500)
  }



  "toInt" must "throw an exception for unordered numerals" in {
    "IIV" == "BOOM"
    "VX" == "BOOM"
    "XXL" == "BOOM"
    "XLC" == "BOOM"
    "LCD" == "BOOM"
    // solve by comparing x and y.  if x < y, all good.  Otherwise it must be one of six permitted subtractive pairs
  }


}
