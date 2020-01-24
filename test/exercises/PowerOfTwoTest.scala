package exercises

import org.scalatest.flatspec.AnyFlatSpec

class PowerOfTwoTest extends AnyFlatSpec {

  val powerOfTwo = new PowerOfTwo()

  "powerOfTwo" must "identify 1 as a power of two" in {
    assert(powerOfTwo.powerOfTwo(1))
  }

  "powerOfTwo" must "identify 2 as a power of two" in {
    assert(powerOfTwo.powerOfTwo(2))
  }

  "powerOfTwo" must "identify 256 as a power of two" in {
    assert(powerOfTwo.powerOfTwo(256))
  }

  "powerOfTwo" must "identify 1024 as a power of two" in {
    assert(powerOfTwo.powerOfTwo(256))
  }

  "powerOfTwo" must "identify 3 as not a power of two" in {
    assert(!powerOfTwo.powerOfTwo(3))
  }

  "powerOfTwo" must "identify MAXINT as not a power of two" in {
    assert(!powerOfTwo.powerOfTwo(Integer.MAX_VALUE))
  }
}