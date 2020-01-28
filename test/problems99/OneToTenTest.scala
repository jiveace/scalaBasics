package problems99

/*
Tests for https://www.thedigitalcatonline.com/blog/2015/04/07/99-scala-problems
 */

import java.util.NoSuchElementException

import org.scalatest.flatspec.AnyFlatSpec

class OneToTenTest extends AnyFlatSpec {

  val oneToTen = new OneToTen

  "lastElement" should "throw exception for null input" in {
    assertThrows[NoSuchElementException](oneToTen.lastElement(null))
  }
  "lastElement" should "throw exception for empty list" in {
    assertThrows[NoSuchElementException](oneToTen.lastElement(List()))
  }
  "lastElement" should "return 1 for [1]" in {
    assert(oneToTen.lastElement(List(1)) == 1)
  }

  "lastElement" should "return 2 for [1,2]" in {
    assert(oneToTen.lastElement(List(1, 2)) == 2)
  }

  "lastElement" should "return 10 for [1,2,3,4,5,6,7,8,9,10]" in {
    assert(oneToTen.lastElement(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) == 10)
  }

  "lastElement" should "return 'Two' for ['One','Two']" in {
    assert(oneToTen.lastElement(List("One", "Two")) == "Two")
  }

  "secondLastElement" should "throw exception for null input" in {
    assertThrows[NoSuchElementException](oneToTen.secondLastElement(null))
  }

  "secondLastElement" should "throw exception for empty list" in {
    assertThrows[NoSuchElementException](oneToTen.secondLastElement(List()))
  }

  "secondLastElement" should "throw exception for [1]" in {
    assertThrows[NoSuchElementException](oneToTen.secondLastElement(List(1)))
  }

  "secondLastElement" should "return 1 for [1,2]" in {
    assert(oneToTen.secondLastElement(List(1, 2)) == 1)
  }

  "secondLastElement" should "return 9 for [1,2,3,4,5,6,7,8,9,10]" in {
    assert(oneToTen.secondLastElement(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) == 9)
  }

  "getIndex" should "throw exception for null input list" in {
    assertThrows[NoSuchElementException](oneToTen.getIndex(null, 0))
  }

  "getIndex" should "throw exception for empty input list" in {
    assertThrows[NoSuchElementException](oneToTen.getIndex(List(), 0))
  }

  "getIndex" should "throw exception for negative index" in {
    assertThrows[IndexOutOfBoundsException](oneToTen.getIndex(List(1), -1))
  }

  "getIndex" should "throw exception for too large an index" in {
    assertThrows[IndexOutOfBoundsException](oneToTen.getIndex(List(1), 1))
  }

  "getIndex" should "return index 0 of a list" in {
    assert(oneToTen.getIndex(List(1), 0) == 1)
    assert(oneToTen.getIndex(List(1, 2, 3), 0) == 1)
    assert(oneToTen.getIndex(List("Yellow"), 0) == "Yellow")
    assert(oneToTen.getIndex(List("Red", "Yellow", "Blue"), 0) == "Red")
  }

  "getIndex" should "return arbitrary of a list" in {
    assert(oneToTen.getIndex(List(1, 2, 3), 2) == 3)
    assert(oneToTen.getIndex(List(1, 2, 3, 4, 5, 6, 7, 8), 4) == 5)
    assert(oneToTen.getIndex(List("yellow", "orange", "green"), 2) == "green")
    assert(oneToTen.getIndex(List(2.3, 4.3, 5.3, 6.3, 7.3, 8.3, 9.3), 5) == 8.3)
  }

  "count" should "count the elements in a list" in {
    assert(oneToTen.count(List()) == 0)
    assert(oneToTen.count(List(1, 2, 3)) == 3)
    assert(oneToTen.count(List(1, 2, 3, 4, 5, 6, 7)) == 7)
    assert(oneToTen.count(List(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0)) == 7)
  }

  "reverse" should "reverse the elements in a list" in {
    assert(oneToTen.reverse(List()) == List())
    assert(oneToTen.reverse(List(1, 2, 3)) == List(3, 2, 1))
    assert(oneToTen.reverse(List(1, 2, 3, 4, 5, 6, 7)) == List(7, 6, 5, 4, 3, 2, 1))
    assert(oneToTen.reverse(List(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0)) == List(7.0, 6.0, 5.0, 4.0, 3.0, 2.0, 1.0))
  }
}
