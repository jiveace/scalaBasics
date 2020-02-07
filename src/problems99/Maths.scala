package problems99

import scala.annotation.tailrec

object Maths {
  def goldbachList(range: Range): List[(Int, (Int, Int))] = {

    range.filter(_ % 2 == 0).filter(_ > 2).map(x => {
      val gl = goldbach(x)
      (x, (gl.head, gl.last))
    }).toList
  }


  def goldbach(x: Int): List[Int] = {

    @tailrec
    def _goldbach(a: Int, b: Int): List[Int] =
      if (isPrime(a) && isPrime(b)) List(a, b)
      else _goldbach(a + 1, b - 1)

    _goldbach(2, x - 2)
  }

  def primes(range: Range): List[Int] =
    range.filter(isPrime(_)).toList


  def primeFactorsMult(x: Int): List[(Int, Int)] = {
    ElevenToTwenty.encodeViaSpan(primeFactors(x))
  }

  def primeFactors(x: Int): List[Int] = {
    def _primeFactors(n: Int): List[Int] =
      if (x % n == 0) primeFactors(x / n) ::: primeFactors(n)
      else _primeFactors(n - 1)

    if (isPrime(x)) List(x)
    else _primeFactors(x / 2)
  }

  def factorise(x: Int): List[Int] = {
    @tailrec
    def foo(x: Int, a: Int = 2, list: List[Int] = Nil): List[Int] = a * a > x match {
      case false if x % a == 0 => foo(x / a, a, list :+ a)
      case false => foo(x, a + 1, list)
      case true => list :+ x
    }

    foo(x)
  }

  def isPrime(x: Int): Boolean = {
    def _isPrime(divisor: Int): Boolean =
      if (divisor <= 1) true
      else x % divisor != 0 && _isPrime(divisor - 1)

    x > 0 && _isPrime(x / 2)
  }


}
