package exercises.redux

import org.scalatest.flatspec.AnyFlatSpec

class MeListTest extends AnyFlatSpec {
  "A MeList with one element" must "have an Empty tail" in {
    val meList = Empty
    val meListWithOneElement: MeList[String] = meList.add("One")
    assert(meListWithOneElement.tail === Empty)
  }

  "A MeList with zero element" must "throw Exception when reading tail" in {
    val meList = Empty
    assertThrows[NoSuchElementException](meList.tail)
  }
}