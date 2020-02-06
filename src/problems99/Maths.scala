package problems99

object Maths {

  def isPrime(x: Int): Boolean = {
    def _isPrime(divisor: Int): Boolean =
      if (divisor <= 1) true
      else x % divisor != 0 && _isPrime(divisor - 1)

    x > 0 && _isPrime(x / 2)
  }
}
