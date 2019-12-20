package lectures.part1basics

import scala.annotation.tailrec

object Recursion extends App {

  def factorial(n: Int): Int =
    if (n < 1) n
    else {
      println("Computing " + n + "! - This requires " + (n - 1) + "!")
      // This expression takes the recursive output and transforms it (*n) - not tail recursion
      val result = n * factorial(n - 1)
      println("Computed " + n + "!")

      result
    }

  factorial(10)

  def factorial2(n: Int): BigInt = {
    def factHelper(x: Int, accumulator: BigInt): BigInt =
      if (x <= 1) accumulator
      else factHelper(x - 1, x * accumulator)

    //This expression is the recursive call, therefor tail recursive
    factHelper(n, 1)
  }

  println(factorial2(5000))


  //Quizzes

  //1
  @tailrec
  def concat(string: String, times: Int, accumulator: String): String =
    if (times == 0) accumulator
    else concat(string, times - 1, accumulator + string)


  println(concat("a", 7, ""))


  //2
  def isPrime(n: Int): Boolean = {
    @tailrec
    def isPrimeUntil(t: Int, isStillPrime: Boolean): Boolean = {
      if (!isStillPrime) false
      else if (t <= 1) true
      else isPrimeUntil(t - 1, n % t != 0)
    }

    isPrimeUntil(n / 2, true)
  }

  @tailrec
  def printPrimes(n: Int): Unit = {
    if (n <= 1) println(n + ":" + isPrime(n))
    else {
      println(n + ":" + isPrime(n))
      printPrimes(n - 1)
    }
  }

  printPrimes(15)


  def fibonacci(n: Int): Int = {
    @tailrec
    def fibonacciHelper(i: Int, last: Int, secondLast: Int): Int =
      if (i > n) last
      else fibonacciHelper(i + 1, last + secondLast, last)

    if (n <= 2) 1
    else fibonacciHelper(3, 1, 1)
  }

  @tailrec
  def printFibs(n: Int): Unit = {
    if (n <= 1) println(n + ":" + fibonacci(n))
    else {
      println(n + ":" + fibonacci(n))
      printFibs(n - 1)
    }
  }

  printFibs(15)
}
