package problems99

import org.scalatest.flatspec.AnyFlatSpec

class TwentyoneToThirtyTest extends AnyFlatSpec {

  "insertAt" should "insert element at the second position" in {
    val result = TwentyoneToThirty.insertAt(List("a", "b", "c", "d"), 2, "alfa")
    assert(result == List("a", "alfa", "b", "c", "d"))
  }

  "insertAt" should "insert element at the zeroth position" in {
    val result = TwentyoneToThirty.insertAt(List("a", "b", "c", "d"), 0, "alfa")
    assert(result == List("alfa", "a", "b", "c", "d"))
  }

  "insertAt" should "insert element at the fifth position" in {
    val result = TwentyoneToThirty.insertAt(List("a", "b", "c", "d"), 5, "alfa")
    assert(result == List("a", "b", "c", "d", "alfa"))
  }

  "range" should "provide a list of int elements from x to y" in {
    assert(TwentyoneToThirty.range(1, 10) == List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
    assert(TwentyoneToThirty.range(-5, 5) == List(-5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5))
    assert(TwentyoneToThirty.range(8, 16) == List(8, 9, 10, 11, 12, 13, 14, 15, 16))
  }

  "extractRandoms" should "randomly select 3 unique elements from the input list" in {
    val input = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    val result: List[Int] = TwentyoneToThirty.randomSelect(input, 3)
    assert(result.length == 3)
    assert(result.length == result.toSet.size)
    assert(input.count(x => result.contains(x)) == 3)
    println(result)
  }

  "lotto" should "randomly select 6 unique elements from within a given range" in {
    val result: List[Int] = TwentyoneToThirty.lotto(6, (1, 49))
    assert(result.length == 6)
    assert(result.length == result.toSet.size)
    assert(result.count(x => x > 1 && x < 49) == 6)
    println(result)
  }
}
