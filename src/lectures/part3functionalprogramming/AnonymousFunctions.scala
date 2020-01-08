package lectures.part3functionalprogramming

object AnonymousFunctions extends App {

  // Anonymous function (LAMBDA)
  // Basically an implementation of APPLY, but with '=>' instead of "="

  // You can specify type to make function definition cleaner
  val doubler: Int => Int = x => x * 2
  // Or use type inference to keep the declaration cleaner
  val tripler = (x: Int) => x * 3

  // Multiple params
  val adder: (Int, Int) => Int = (x, y) => x + y

  // Nae params.  Prick.
  val engage: () => Int = () => 3

  // Lambdas with {}
  val stringToInt = { (str: String) =>
    str.toInt
  }

  // MOAR Syntactic Sugar
  val cuteIncrementer: Int => Int = x => x + 1
  val sexyIncrementer: Int => Int = _ + 1
  println(cuteIncrementer(47) == sexyIncrementer(47))

  val cuteAdder: (Int, Int) => Int = (a, b) => a + b
  val sexyAdder: (Int, Int) => Int = _ + _
  println(cuteAdder(47, 2) == sexyAdder(47, 2))

  /*
  Quizzes
  1. List replace all Function[] calls with lambdas
  2. refactor doubler from last lecture to vbe anonymous
   */
}
