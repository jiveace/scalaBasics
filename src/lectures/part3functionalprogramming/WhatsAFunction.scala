package lectures.part3functionalprogramming

object WhatsAFunction extends App {

  // DREAM: use functions as first class citizens
  // PROBLEM: JVM is designed for OO

  trait MyFunction[A, B] {
    def apply(element: A): B
  }

  val doubler = new MyFunction[Int, Int] {
    override def apply(element: Int): Int = element * 2
  }

  println(doubler(2))

  // function types = Funtion1[A, B], Funtion2[A, B, C], Funtion3[A, B, C, D]
  val stringToIntConverter = new Function1[String, Int] {
    override def apply(string: String): Int = string.toInt
  }

  println(stringToIntConverter("3") + 4)

  val adder: ((Int, Int) => Int) = new Function2[Int, Int, Int] {
    override def apply(a: Int, b: Int): Int = a + b
  }

  // Function types Function2[A, B, R] === (A,B) => R
  // ALL SCALA FUNCTIONS ARE OBJECTS

  /* Exercises
    1. function which concatenates 2 strings
    2. transform myPredicate (in redux) and MyTransformer into functions
    3. Define a function which tajkes an argument (int) and returns another function from int => int
       - what's the type of the function
       - how to do it
  */
}


