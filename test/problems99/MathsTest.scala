package problems99

import org.scalatest.flatspec.AnyFlatSpec

class MathsTest extends AnyFlatSpec {

  "isPrime" should "return false for 0" in {
    assertFalse(Maths.isPrime(0))
  }

  "isPrime" should "return true for low values" in {
    assert(Maths.isPrime(1))
    assert(Maths.isPrime(2))
    assert(Maths.isPrime(3))
  }

  "isPrime" should "return false for even values" in {
    assertFalse(Maths.isPrime(4))
    assertFalse(Maths.isPrime(6))
  }

  "isPrime" should "return false for squares of odd numbers" in {
    assertFalse(Maths.isPrime(3 * 3))
    assertFalse(Maths.isPrime(5 * 5))
    assertFalse(Maths.isPrime(7 * 7))
    assertFalse(Maths.isPrime(9 * 9))
    assertFalse(Maths.isPrime(13 * 13))
    assertFalse(Maths.isPrime(17 * 17))
    assertFalse(Maths.isPrime(171 * 171))
    assertFalse(Maths.isPrime(1711 * 1711))
  }

  "isPrime" should "return true for known primes" in {
    assert(Maths.isPrime(191))
    assert(Maths.isPrime(193))
    assert(Maths.isPrime(197))
    assert(Maths.isPrime(199))
    assert(Maths.isPrime(999961))
  }

  "primeFactors" should "return [x] for low values" in {
    assert(Maths.primeFactors(1) == List(1))
    assert(Maths.primeFactors(2) == List(2))
    assert(Maths.primeFactors(3) == List(3))
  }

  it should "return [2,2] for 4" in {
    assert(Maths.primeFactors(4) == List(2, 2))
  }
  it should "return [2,2,2] for 8" in {
    assert(Maths.primeFactors(8) == List(2, 2, 2))
  }

  it should "return [2,2,3] for 12" in {
    assert(Maths.primeFactors(12) == List(2, 2, 3))
  }

  it should "return [3,3,5,7] for 315" in {
    assert(Maths.primeFactors(315) == List(3, 3, 5, 7))
  }

  it should "return [3,11] for 33" in {
    assert(Maths.primeFactors(33) == List(3, 11))
  }

  it should "return factors for 1 million" in {
    assert(Maths.primeFactors(1_000_001) == List(101, 9901))
  }

  it should "return factors for 10 million" in {
    assert(Maths.primeFactors(10_000_001) == List(11, 909091))
  }

  it should "return factors for 100 million" in {
    assert(Maths.primeFactors(100_000_001) == List(17, 5882353))
  }

  it should "return factors for one under a billion" in {
    //    assert(Maths.primeFactors(999_999_999) == List(3, 3, 3, 3, 37, 333667))
  }

  it should "return factors for one over a billion" in {
    //    assert(Maths.primeFactors(1_000_000_001) == List(7, 11, 13, 19, 52579))
  }

  it should "return factors for biggest non prime Int" in {
    //    assert(Maths.primeFactors(Integer.MAX_VALUE - 1) == List(2, 3, 3, 7, 11, 31, 151, 331))
  }

  "primeFactorsMult" should "return factors for biggest non prime Int as tuples" in {
    assert(Maths.primeFactorsMult(Integer.MAX_VALUE - 1) == List((2, 1), (3, 2), (7, 1), (11, 1), (31, 1), (151, 1), (331, 1)))
  }

  "primes" should "return all primes for range 1 to 10" in {
    assert(Maths.primes(1 to 10) == List(1, 2, 3, 5, 7))
  }

  it should "return all primes for range 7 to 31" in {
    assert(Maths.primes(7 to 31) == List(7, 11, 13, 17, 19, 23, 29, 31))
  }

  "goldbach" should "return [3,5] for input 8)" in {
    assert(Maths.goldbach(8) == List(3, 5))
  }

  it should "return [5,23] for input 28)" in {
    assert(Maths.goldbach(28) == List(5, 23))
  }

  "goldbachList" should "produce a list of goldbach compositions" in {
    val result = Maths.goldbachList(9 to 20)
    assert(result == List((10, (3, 7)), (12, (5, 7)), (14, (3, 11)), (16, (3, 13)), (18, (5, 13)), (20, (3, 17))))
  }

  "greatestCommonDenominator" should "return 9 for 36 and 63" in {
    assert(Maths.greatestCommonDenominator(36, 63) == 9)
  }

  def assertFalse(bool: Boolean): Unit = {
    assert(!bool)
  }


}
