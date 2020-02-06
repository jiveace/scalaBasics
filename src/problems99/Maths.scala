package problems99

object Maths {
  def primeFactors(x: Int): List[Int] = {
    def _primeFactors(n: Int): List[Int] =
      if (x % n == 0) primeFactors(x / n) ::: primeFactors(n)
      else _primeFactors(n - 1)

    if (isPrime(x)) List(x)
    else _primeFactors(x / 2)
  }

  def isPrime(x: Int): Boolean = {
    def _isPrime(divisor: Int): Boolean =
      if (divisor <= 1) true
      else x % divisor != 0 && _isPrime(divisor - 1)

    x > 0 && _isPrime(x / 2)
  }


}
