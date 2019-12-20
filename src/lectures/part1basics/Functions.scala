package lectures.part1basics

object Functions extends App {

  def function(a: String, b: Int) = {
    a + " " + b
  }

  def paramaterlessFunction(): Int = 8

  // Next 2 lines identical
  println(paramaterlessFunction())
  println(paramaterlessFunction)

  def repeatedFunction(string: String, n: Int): String = {
    if (n == 1) string
    else string + repeatedFunction(string, n - 1)
  }

  println(repeatedFunction("hello", 3))
  // WHEN YOU NEED LOOPS, USE RECURSION


  def functionWithSideEffects(string: String): Unit = println(string)

  def bigFunction(n: Int): Int = {
    def innerFunction(a: String, b: Int, c: Int) = b + c

    innerFunction("Have", 3, 3)
  }

  println(bigFunction(7))


  // Quizzes
  // 1
  def greeting(name: String, age: Int) = "Hi, my name is " + name + " and I am " + age + " years old."

  //2
  def factorial(n: Int): Int =
    if (n < 1) n
    else n * factorial(n - 1)


  //3
  def fibonacci(n: Int): Int =
    if (n == 1 || n == 2) 1
    else fibonacci(n - 2) + fibonacci(n - 1)


  //4
  def isPrime(n: Int): Boolean = {
    def isPrimeUntil(t: Int): Boolean =
      if (t <= 1) true
      else n % t != 0 && isPrimeUntil(t - 1)

    isPrimeUntil(n / 2)
  }

  println(greeting("Iain", 6))

  println("factorial(1): " +  factorial(1))

  println("fibonacci(8): " +  fibonacci(8))

  println("isPrime(4): " +  isPrime(4))
  println("isPrime(9): " +  isPrime(9))
}
