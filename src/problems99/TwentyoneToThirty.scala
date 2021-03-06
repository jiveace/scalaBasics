package problems99

import scala.util.Random

object TwentyoneToThirty {

  // 26
  def combinations[A](list: List[A], x: Int): List[List[A]] = {
    list match {
      case Nil => List()
      case h :: tail if x == 1 => list.map(List(_))
      case h :: tail => combinations(tail, x - 1).map(h :: _) ::: combinations(tail, x)
    }
  }

  // 25
  def randomise[A](input: List[A]): List[A] = {
    randomSelect(input, input.length)
  }

  // 24
  def lotto(n: Int, rangeValues: (Int, Int)): List[Int] =
    randomSelect(range(rangeValues._1, rangeValues._2), n)

  // 23
  def randomSelect[A](list: List[A], x: Int): List[A] = {
    def _randomSelect[A](acc: List[A], pool: List[A]): List[A] =
      if (acc.size == x) acc
      else {
        val (remainingList, plucked) = ElevenToTwenty.removeAt(pool, Random.nextInt(pool.size) + 1)
        _randomSelect(acc :+ plucked, remainingList)
      }

    _randomSelect(List(), list)
  }

  // 22
  def insertAt[A](list: List[A], slot: Int, value: A): List[A] =
    (list.take(slot - 1) :+ value) ::: list.takeRight(list.length + 1 - slot)

  // 21
  def range(x: Int, y: Int): List[Int] = {
    def _range(_list: List[Int], next: Int): List[Int] = {
      if (next > y) _list
      else _range(_list :+ next, next + 1)
    }

    _range(List(x), x + 1)
  }
}
