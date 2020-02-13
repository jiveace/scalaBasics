package problems99

import org.scalatest.funspec.AnyFunSpec
import problems99.Roman.toInt

class RomanTest extends AnyFunSpec {

  describe("toInt") {
    describe("Badly Formed Input") {

      it("must return 0 for null input") {
        assert(toInt(null) == 0)
      }

      it("must return 0 for empty input") {
        assert(toInt("") == 0)
      }

      it("must throw exception for unsupported denomination") {
        assertThrows[IllegalStateException](toInt("P") == 0)
        assertThrows[IllegalStateException](toInt("A") == 0)
        assertThrows[IllegalStateException](toInt("R") == 0)
        assertThrows[IllegalStateException](toInt("T") == 0)
      }
    }

    describe("handling 'I's") {
      it("must count Is properly") {
        assert(toInt("I") == 1)
        assert(toInt("III") == 3)
        assert(toInt("IIIII") == 5)
      }

      it("must not allow more then nine Is in a row") {
        assertThrows[IllegalStateException](toInt("IIIIIIIIII"))
        assertThrows[IllegalStateException](toInt("IIIIIIIIIIIII"))
      }
    }

    describe("Handling 'V's") {
      it("must count V as five") {
        assert(toInt("V") == 5)
      }

      it("must count V followed by Is") {
        assert(toInt("VI") == 6)
        assert(toInt("VIIII") == 9)
        assert(toInt("VIIIIIIII") == 13)
      }

      it("must count V preceded by I") {
        assert(toInt("IV") == 4)
      }

      it("must throw an exception for multiple Vs") {
        assertThrows[IllegalStateException](toInt("VV"))
        assertThrows[IllegalStateException](toInt("VIV"))
      }
    }

    describe("Handling 'X's") {

      it("must count X as ten") {
        assert(toInt("X") == 10)
        assert(toInt("XX") == 20)
        assert(toInt("XXX") == 30)
      }

      it("must count X preceded by a I") {
        assert(toInt("IX") == 9)
      }

      it("must count X followed by Is") {
        assert(toInt("XI") == 11)
        assert(toInt("XIIII") == 14)
        assert(toInt("XIIIIIIII") == 18)
      }

      it("must count X followed by Vs and Is") {
        assert(toInt("XVI") == 16)
        assert(toInt("XIV") == 14)
        assert(toInt("XVIII") == 18)
      }


      it("must not allow more than 9 consecutive Xs") {
        assert(toInt("XXXXXXXXX") == 90)
        assertThrows[IllegalStateException](toInt("XXXXXXXXXX"))
        assertThrows[IllegalStateException](toInt("XXXXXXXXXXXX"))
      }
    }

    describe("Handling 'L's") {

      it("must count L as fifty") {
        assert(toInt("L") == 50)
      }

      it("must throw an exception for multiple Ls") {
        assertThrows[IllegalStateException](toInt("LL"))
        assertThrows[IllegalStateException](toInt("LIL"))
      }

      it("must count Ls followed by Is") {
        assert(toInt("LI") == 51)
        assert(toInt("LIIII") == 54)
        assert(toInt("LIIIIIIII") == 58)
      }

      it("must count L followed by Vs and Is") {
        assert(toInt("LVI") == 56)
        assert(toInt("LIV") == 54)
        assert(toInt("LVIII") == 58)
      }

      it("must count L followed by Xs, Vs and Is") {
        assert(toInt("LXVI") == 66)
        assert(toInt("LXXIV") == 74)
        assert(toInt("LXXVIII") == 78)
      }

      it("must count L preceded by a smaller numeral") {
        assert(toInt("IL") == 49)
        assert(toInt("VL") == 45)
        assert(toInt("XL") == 40)
      }
    }

    describe("Handling 'C's") {

      it("must count C as one hundred") {
        assert(toInt("C") == 100)
        assert(toInt("CC") == 200)
        assert(toInt("CCCCC") == 500)
      }

      it("must not allow more than 9 consecutive Cs") {
        assert(toInt("CCCCCCCCC") == 900)
        assertThrows[IllegalStateException](toInt("CCCCCCCCCC"))
        assertThrows[IllegalStateException](toInt("CCCCCCCCCCCC"))
      }

      it("must count C followed by Is") {
        assert(toInt("CI") == 101)
        assert(toInt("CIIII") == 104)
        assert(toInt("CIIIIIIII") == 108)
      }

      it("must count C followed by Vs and Is") {
        assert(toInt("CVI") == 106)
        assert(toInt("CIV") == 104)
        assert(toInt("CVIII") == 108)
      }

      it("must count C followed by Xs, Vs and Is") {
        assert(toInt("CXVI") == 116)
        assert(toInt("CXXIV") == 124)
        assert(toInt("CXXVIII") == 128)
      }

      it("must count C followed by Ls, Xs, Vs and Is") {
        assert(toInt("CLXVI") == 166)
        assert(toInt("CLXXIV") == 174)
        assert(toInt("CLXXVIII") == 178)
        assert(toInt("CCLXXVIII") == 278)
      }

      it("must count C preceded by a smaller numeral") {
        assert(toInt("IC") == 99)
        assert(toInt("VC") == 95)
        assert(toInt("XC") == 90)
        assert(toInt("LC") == 50)
      }
    }

    describe("Handling 'D's") {

      it("must count D as five hundred") {
        assert(toInt("D") == 500)
      }

      it("must throw an exception for multiple Ds") {
        assertThrows[IllegalStateException](toInt("DD"))
        assertThrows[IllegalStateException](toInt("DID"))
      }


      it("must count D followed by Is") {
        assert(toInt("DI") == 501)
        assert(toInt("DIIII") == 504)
        assert(toInt("DIIIIIIII") == 508)
      }

      it("must count D followed by Vs and Is") {
        assert(toInt("DVI") == 506)
        assert(toInt("DIV") == 504)
        assert(toInt("DVIII") == 508)
      }

      it("must count D followed by Xs, Vs and Is") {
        assert(toInt("DXVI") == 516)
        assert(toInt("DXXIV") == 524)
        assert(toInt("DXXVIII") == 528)
      }

      it("must count D followed by Ls, Xs, Vs and Is") {
        assert(toInt("DLXVI") == 566)
        assert(toInt("DLXXIV") == 574)
        assert(toInt("DLXXVIII") == 578)
      }

      it("must count D followed by Cs, Ls, Xs, Vs and Is") {
        assert(toInt("DCLXVI") == 666)
        assert(toInt("DCLXXIV") == 674)
        assert(toInt("DCLXXVIII") == 678)
        assert(toInt("DCCLXXVIII") == 778)
      }

      it("must count D preceded by a smaller numeral") {
        assert(toInt("ID") == 499)
        assert(toInt("VD") == 495)
        assert(toInt("XD") == 490)
        assert(toInt("LD") == 450)
        assert(toInt("CD") == 400)
      }
    }

    describe("Handling 'M's") {

      it("must count M as one thousand") {
        assert(toInt("M") == 1000)
      }

      it("must count M followed by Is") {
        assert(toInt("MI") == 1001)
        assert(toInt("MIIII") == 1004)
        assert(toInt("MIIIIIIII") == 1008)
      }

      it("must count M followed by Vs and Is") {
        assert(toInt("MVI") == 1006)
        assert(toInt("MIV") == 1004)
        assert(toInt("MVIII") == 1008)
      }

      it("must count M followed by Xs, Vs and Is") {
        assert(toInt("MXVI") == 1016)
        assert(toInt("MXXIV") == 1024)
        assert(toInt("MXXVIII") == 1028)
      }

      it("must count M followed by Ls, Xs, Vs and Is") {
        assert(toInt("MLXVI") == 1066)
        assert(toInt("MLXXIV") == 1074)
        assert(toInt("MLXXVIII") == 1078)
      }

      it("must count M followed by Cs, Ls, Xs, Vs and Is") {
        assert(toInt("MCLXVI") == 1166)
        assert(toInt("MCLXXIV") == 1174)
        assert(toInt("MCLXXVIII") == 1178)
        assert(toInt("MCCLXXVIII") == 1278)
      }

      it("must count M followed by D's, Cs, Ls, Xs, Vs and Is") {
        assert(toInt("MDCLXVI") == 1666)
        assert(toInt("MDCLXXIV") == 1674)
        assert(toInt("MDCLXXVIII") == 1678)
        assert(toInt("MMMDCCLXXVIII") == 3778)
      }

      it("must count M preceded by a smaller numeral") {
        assert(toInt("IM") == 999)
        assert(toInt("VM") == 995)
        assert(toInt("XM") == 990)
        assert(toInt("LM") == 950)
        assert(toInt("CM") == 900)
        assert(toInt("DM") == 500)
      }
    }

    describe("Illegally Ordered Input Numerals") {

      it("must throw an exception for unordered numerals") {
        "IIV" == "BOOM"
        "VX" == "BOOM"
        "XXL" == "BOOM"
        "XLC" == "BOOM"
        "LCD" == "BOOM"
        // solve by comparing x and y.  if x < y, all good.  Otherwise it must be one of six permitted subtractive pairs
      }
    }
  }
}
