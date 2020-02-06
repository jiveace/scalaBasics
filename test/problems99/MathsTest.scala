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

  "primeFactors" should "return [2,2] for 4" in {
    assert(Maths.primeFactors(4) == List(2, 2))
  }
  "primeFactors" should "return [2,2,2] for 8" in {
    assert(Maths.primeFactors(8) == List(2, 2, 2))
  }

  "primeFactors" should "return [2,2,3] for 12" in {
    assert(Maths.primeFactors(12) == List(2, 2, 3))
  }

  "primeFactors" should "return [3,3,5,7] for 315" in {
    assert(Maths.primeFactors(315) == List(3, 3, 5, 7))
  }

  "primeFactors" should "return [3,11] for 33" in {
    assert(Maths.primeFactors(33) == List(3, 11))
  }

  "primeFactors" should "return factors for large values" in {
    assert(Maths.primeFactors(1_000_001) == List(101, 9901))
    assert(Maths.primeFactors(10_000_001) == List(11, 909091))
    assert(Maths.primeFactors(100_000_001) == List(17, 5882353))
    assert(Maths.primeFactors(1_000_000_001) == List(7, 11, 13, 19, 52579))
    assert(Maths.primeFactors(999_999_999) == List(3, 3, 3, 3, 37, 333667))
  }


  def assertFalse(bool: Boolean): Unit = {
    assert(!bool)
  }


}
