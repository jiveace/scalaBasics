package lectures.part3functionalprogramming

object HOFsAndCurries extends App {


  // function that applies a function n times over value x
  // nTimes(f, n, x)
  // nTimes(f, 3, x) = f(f(f(x)))
  def nTimes(f: Int => Int, n: Int, x: Int): Int =
    if (n <= 0) x
    else nTimes(f, n - 1, f(x))

  // Add one to 3 3 times
  println(nTimes(_ + 1, 3, 3))

  // Double 1 four times
  println(nTimes(_ * 2, 4, 1))

  // ntb(f,n) = x => f(f(f...(x)))
  // increment10 = ntb(plusOne, 10) = x => plusOne(plusOne...(x))
  // val y = increment10(1)
  def nTimesBetter(f: Int => Int, n: Int): (Int => Int) =
    if (n <= 0) (x: Int) => x
    else (x: Int) => nTimesBetter(f, n - 1)(f(x))

  val increment10 = nTimesBetter(_ + 1, 10)
  println(increment10(2))

  // Ladies and gentlemen... THE CURRIES
  val superAdder: Int => (Int => Int) = (x: Int) => (y: Int) => x + y

  val threeAdder = superAdder(3)
  val fourAdder = superAdder(4)
  println(threeAdder(5) == 5 + 3)
  println(fourAdder(5) == 5 + 4)
  println(superAdder(6)(5) == 5 + 6)

  // functions with multiple parameter lists
  def curriedFormatter(c: String)(x: Double): String = c.format(x)

  val standardFormat: (Double => String) = curriedFormatter("%4.2f")
  val preciseFormat: (Double => String) = curriedFormatter("%4.10f")
  println(standardFormat(Math.PI))
  println(preciseFormat(Math.PI))
  println(curriedFormatter("%2.1f")(Math.PI))
}