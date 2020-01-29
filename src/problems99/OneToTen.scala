package problems99

/*
Test Driven Implementations of https://www.thedigitalcatonline.com/blog/2015/04/07/99-scala-problems
 */

import java.util.NoSuchElementException

class OneToTen {
  def compress[A](n: List[A]): List[A] = {

    def _compress(_n: List[A], accumulator: List[A]): List[A] =
      if (_n.isEmpty) accumulator
      else if (_n.size == 1) accumulator ::: List(_n.head)
      else if (_n.head == _n.tail.head) _compress(_n.tail, accumulator)
      else _compress(_n.tail, accumulator ::: List(_n.head))

    if (n == null) throw new IllegalStateException("Cannot compress null list")
    else _compress(n, List())
  }

  def flatten(n: List[Any]): List[Any] = {

    def _flatten(res: List[Any], remainder: List[Any]): List[Any] = {
      if (remainder.isEmpty) res
      else remainder.head match {
        case list: List[Any] => _flatten(res ::: flatten(list), remainder.tail)
        case singular => _flatten(res ::: List(singular), remainder.tail)
      }
    }

    if (n == null) throw new IllegalStateException("Cannot flatten null list")
    else _flatten(List(), n)
  }

  def isPalindrome[A](n: List[A]): Boolean = {
    if (n == null) throw new NoSuchElementException
    if (n.size < 2) true
    else n.head.equals(n.last) && isPalindrome(n.tail.init)
  }

  def reverse[A](n: List[A]): List[A] = {
    def helper[A](oldList: List[A], newList: List[A]): List[A] =
      if (oldList.isEmpty) newList
      else helper(oldList.tail, oldList.head +: newList)

    if (n == null) throw new NoSuchElementException
    helper(n, List())
  }

  def count[A](n: List[A]): Int = {
    def helper(subList: List[A], acc: Int): Int =
      if (subList.isEmpty) acc
      else helper(subList.tail, acc + 1)

    if (n == null) throw new NoSuchElementException
    helper(n, 0)
  }

  def getIndex[A](n: List[A], index: Int): A =
    if (n == null || n.isEmpty) throw new NoSuchElementException
    else if (index < 0 || index >= n.length) throw new IndexOutOfBoundsException
    else if (index == 0) n.head
    else getIndex(n.tail, index - 1)

  def lastElement[A](n: List[A]): A = {
    if (n == null || n.isEmpty) throw new NoSuchElementException
    else if (n.tail.isEmpty) n.head
    else lastElement(n.tail)
  }

  def secondLastElement[A](n: List[A]): A = {
    if (n == null || n.size < 2) throw new NoSuchElementException
    else if (n.tail.size == 1) n.head
    else secondLastElement(n.tail)
  }
}
