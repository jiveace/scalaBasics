package problems99

object Maths {

  def isPrime(x: Int): Boolean = {
    def _isPrime(divisor: Int): Boolean =
      if (divisor == 1) true
      else if (x % divisor == 0) false
      else {
        _isPrime(divisor - 1)
      }

    x match {
      case 0 => false
      case _ => _isPrime(Math.ceil(x / 2.0).toInt)
    }
  }
}
