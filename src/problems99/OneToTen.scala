package problems99

/*
Test Driven Implementations of https://www.thedigitalcatonline.com/blog/2015/04/07/99-scala-problems
 */

import java.util.NoSuchElementException

class OneToTen {
  def reverse[A](value: List[A]): List[A] = {
    def helper[A](oldList: List[A], newList: List[A]): List[A] =
      if (oldList.isEmpty) newList
      else helper(oldList.tail, oldList.head +: newList)

    helper(value, List())
  }

  def count[A](value: List[A]): Int = {
    def helper(subList: List[A], acc: Int): Int =
      if (subList.isEmpty) acc
      else helper(subList.tail, acc + 1)

    helper(value, 0)
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
