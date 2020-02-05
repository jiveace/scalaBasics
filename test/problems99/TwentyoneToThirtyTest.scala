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
    val (lowerBound, upperBound, n) = (1, 49, 6)
    val result: List[Int] = TwentyoneToThirty.lotto(n, (lowerBound, upperBound))
    assert(result.length == n)
    assert(result.length == result.toSet.size)
    assert(result.count(x => x >= lowerBound && x <= upperBound) == n)
    println(result)
  }

  "randomise" should "return all elements of the input list in a random order" in {
    def _test[A](input: List[A]) = {
      val result = TwentyoneToThirty.randomise(input);
      assert(result.length == input.length)
      assert(result.length == result.toSet.size)
      assert(input.count(x => result.contains(x)) == input.length)
      println(result)
    }

    _test(List())
    _test(TwentyoneToThirty.range(1, 100))
    _test(TwentyoneToThirty.range(50, 100))
    _test(List("A", "B", "C", "D", "E", "F", "G"))
    _test(List("Egg", "Bag", "Of", "Ham", "Global", "Gold", "LindyHopIchiban"))
  }

  "combinations" must "return an empty list for empty input" in {
    assert(TwentyoneToThirty.combinations(List(), 1) == List())
  }

  "combinations" must "return the input list for a single element list" in {
    TwentyoneToThirty.combinations(List(1), 1)
    TwentyoneToThirty.combinations(List("A"), 1)
    assert(TwentyoneToThirty.combinations(List(1), 1) == List(List(1)))
    assert(TwentyoneToThirty.combinations(List("A"), 1) == List(List("A")))

  }

  "combinations" must "return 20 combinations for 3 combos of a 6 element list" in {
    assert(TwentyoneToThirty.combinations(List("a", "b", "c", "d", "e", "f"), 3).size == 20)
    println(TwentyoneToThirty.combinations(List("a", "b", "c", "d", "e", "f"), 3))
  }
}
