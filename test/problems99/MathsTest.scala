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

  def assertFalse(bool: Boolean): Unit = {
    assert(!bool)
  }

}
