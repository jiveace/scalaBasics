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

  "count" should "throw exception for null input" in {
    assertThrows[NoSuchElementException](oneToTen.count(null))
  }

  "count" should "count the elements in a list" in {
    assert(oneToTen.count(List()) == 0)
    assert(oneToTen.count(List(1, 2, 3)) == 3)
    assert(oneToTen.count(List(1, 2, 3, 4, 5, 6, 7)) == 7)
    assert(oneToTen.count(List(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0)) == 7)
  }

  "reverse" should "throw exception for null input" in {
    assertThrows[NoSuchElementException](oneToTen.reverse(null))
  }

  "reverse" should "reverse the elements in a list" in {
    assert(oneToTen.reverse(List()) == List())
    assert(oneToTen.reverse(List(1, 2, 3)) == List(3, 2, 1))
    assert(oneToTen.reverse(List(1, 2, 3, 4, 5, 6, 7)) == List(7, 6, 5, 4, 3, 2, 1))
    assert(oneToTen.reverse(List(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0)) == List(7.0, 6.0, 5.0, 4.0, 3.0, 2.0, 1.0))
  }

  "isPalindrome" should "throw exception for null input" in {
    assertThrows[NoSuchElementException](oneToTen.isPalindrome(null))
  }

  "isPalindrome" should "identify if a list is a palindrome" in {
    assert(oneToTen.isPalindrome(List()) == true)

    assert(oneToTen.isPalindrome(List(1, 2, 3)) == false)
    assert(oneToTen.isPalindrome(List(1, 2, 3, 4, 5, 6, 7)) == false)
    assert(oneToTen.isPalindrome(List(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0)) == false)

    assert(oneToTen.isPalindrome(List(1, 2, 1)) == true)
    assert(oneToTen.isPalindrome(List(1, 2, 3, 4, 3, 2, 1)) == true)
    assert(oneToTen.isPalindrome(List(1.0, 2.0, 3.0, 4.0, 3.0, 2.0, 1.0)) == true)
  }

  "flatten" should "throw exception for null input" in {
    assertThrows[IllegalStateException](oneToTen.flatten(null))
  }

  "flatten" should "throw return empty list for empty input" in {
    assert(oneToTen.flatten(List()) == List())
  }

  "flatten" should "support single elements" in {
    assert(oneToTen.flatten(List(1, 2, 3, 4, 5)) == List(1, 2, 3, 4, 5))
  }

  "flatten" should "support top level lists elements" in {
    assert(oneToTen.flatten(List(1, List(2, 3), 4, 5)) == List(1, 2, 3, 4, 5))
    assert(oneToTen.flatten(List(10, List(20, 30), List(40, 50))) == List(10, 20, 30, 40, 50))
    assert(oneToTen.flatten(List(List("1"), List("2", "3"), List("4", "5"))) == List("1", "2", "3", "4", "5"))
  }

  "flatten" should "support mixed types" in {
    assert(oneToTen.flatten(List(1, List("2", 3), 4, "5")) == List(1, "2", 3, 4, "5"))
    assert(oneToTen.flatten(List(10, List(20, 30), List(false, false))) == List(10, 20, 30, false, false))
  }

  "flatten" should "supports nested lists" in {
    assert(oneToTen.flatten(List(1, List(2, List(3)), 4, 5)) == List(1, 2, 3, 4, 5))
    assert(oneToTen.flatten(List(10, List(List(20), List(30)), List(40, 50))) == List(10, 20, 30, 40, 50))
    assert(oneToTen.flatten(List(List(List(List(List(List("1"))))))) == List("1"))
  }

  "compress" should "throw exception for null input" in {
    assertThrows[IllegalStateException](oneToTen.compress(null))
  }

  "compress" should "throw return empty list for empty input" in {
    assert(oneToTen.compress(List()) == List())
  }

  "compress" should "not alter list with non consecutive duplicates" in {
    assert(oneToTen.compress(List(1, 2, 3, 4, 5, 1, 2, 3, 4, 5)) == List(1, 2, 3, 4, 5, 1, 2, 3, 4, 5))
    assert(oneToTen.compress(List("one", "two", "one")) == List("one", "two", "one"))
  }

  "compress" should "compress consecutive duplicates" in {
    assert(oneToTen.compress(List(1, 1)) == List(1))
    assert(oneToTen.compress(List(1, 1, 2, 2)) == List(1, 2))
    assert(oneToTen.compress(List(1, 1, 2, 3, 3)) == List(1, 2, 3))
    assert(oneToTen.compress(List(0, 1, 1, 2, 3, 3, 4)) == List(0, 1, 2, 3, 4))

    assert(oneToTen.compress(List(0.0, 1.0, 1.0, 2.0, 3.0, 3.0, 4.0)) == List(0.0, 1.0, 2.0, 3.0, 4.0))
    assert(oneToTen.compress(List("0", "1", "1", "2", "3", "3", "4")) == List("0", "1", "2", "3", "4"))
  }

  "compress" should "not round doubles when considering duplicates" in {
    assert(oneToTen.compress(List(0.0, 1.0, 1.1, 1.1, 3.0, 3.0, 3.1)) == List(0.0, 1.0, 1.1, 3.0, 3.1))

  }

  "compressToList" should "throw exception for null input" in {
    assertThrows[IllegalStateException](oneToTen.compressToList(null))
  }

  "compressToList" should "throw return list containing an empty list for empty input" in {
    assert(oneToTen.compressToList(List()) == List())
  }

  "compressToList" should "throw convert a single element to a list" in {
    assert(oneToTen.compressToList(List(1)) == List(List(1)))
    assert(oneToTen.compressToList(List(1, 2)) == List(List(1), List(2)))
    assert(oneToTen.compressToList(List("A", "B", "C")) == List(List("A"), List("B"), List("C")))
  }

  "compressToList" should "throw convert repeated elements to a single list" in {
    assert(oneToTen.compressToList(List(1, 1)) == List(List(1, 1)))
    assert(oneToTen.compressToList(List(1, 1, 2, 2)) == List(List(1, 1), List(2, 2)))
    assert(oneToTen.compressToList(List(1, 1, 1, 1, 2, 2, 2, 3, 3, 4)) == List(List(1, 1, 1, 1), List(2, 2, 2), List(3, 3), List(4)))
  }

  "compressToList" should "handles single elements and lists" in {
    assert(oneToTen.compressToList(List(1, 1, 2)) == List(List(1, 1), List(2)))
    assert(oneToTen.compressToList(List("1", "1", "2", "3", "3")) == List(List("1", "1"), List("2"), List("3", "3")))
    assert(oneToTen.compressToList(List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')) == List(List('a', 'a', 'a', 'a'), List('b'), List('c', 'c'), List('a', 'a'), List('d'), List('e', 'e', 'e', 'e')))
  }

  "encode" should "throw exception for null input" in {
    assertThrows[IllegalStateException](oneToTen.encode(null))
  }

  "encode" should "throw exception for empty input" in {
    assertThrows[IllegalStateException](oneToTen.encode(List()) == List())
  }

  "encode" should "encode a single element to a tuple" in {
    assert(oneToTen.encode(List(1)) == List((1, 1)))
    assert(oneToTen.encode(List(1, 2)) == List((1, 1), (2, 1)))
    assert(oneToTen.encode(List("A", "B", "C")) == List(("A", 1), ("B", 1), ("C", 1)))
  }

  "encode" should "encode consecutive identical elements to the same tuple" in {
    assert(oneToTen.encode(List("A", "A")) == List(("A", 2)))
    assert(oneToTen.encode(List(1, 1, 1, 2, 2, 2)) == List((1, 3), (2, 3)))
  }

  "encode" should "encode non consecutive identical elements to a different tuple" in {
    assert(oneToTen.encode(List("A", "B", "A")) == List(("A", 1), ("B", 1), ("A", 1)))
    assert(oneToTen.encode(List('t', 'a', 't', 'a', 'b', 't')) == List(('t', 1), ('a', 1), ('t', 1), ('a', 1), ('b', 1), ('t', 1)))
  }
}
