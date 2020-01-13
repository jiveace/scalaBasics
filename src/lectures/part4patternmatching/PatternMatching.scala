package lectures.part4patternmatching

import scala.util.Random

object PatternMatching extends App {

  // switch on steroids
  val random = new Random
  val x = random.nextInt(10)

  val description = x match {
    case 1 => "The ONE"
    case 2 => "Double or Nothing"
    case 3 => "Holy Trinity"
    case _ => "Creationists can't count this high"
  }

  println(s"$x $description")

  // Decompose values
  case class Person(name: String, age: Int)

  val bob = Person("Bob", 20)

  val greeting = bob match {
    case Person(n, a) if a < 21 => s"Hi, my name is $n and I am $a years old, but I can't drink in the USA"
    case Person(n, a) => s"Hi, my name is $n and I am $a years old"
    case _ => "I don't know who I am.  DESTINY HELP ME"
  }

  println(greeting)

  // PM on sealed hierarchies
  sealed class Animal

  case class Dog(breed: String) extends Animal

  case class Parrot(greeting: String) extends Animal

  val animal: Animal = Dog("Steve French")
  val observation = animal match {
    case Dog(somebreed) => println(s"Matched a dog of the breed $somebreed")
    case Parrot(greeting) => println(s"Squawk $greeting")
  }

  println(observation)


  /* Quizzes
  take an expr and return human readable form

  Sum(Number(2), Number(3)) => 2 + 3
  Sum(Number(2), Number(3), Number(4)) => 2 + 3 + 4
  Prod(Sum(Number(2),Number(1)), Number(3)) = (2 + 1) * 3
  Sum(Prod(Number(2),Number(1)), Number(3)) = 2 * 1 + 3
   */
  trait Expr

  case class Number(n: Int) extends Expr {
    def value = n
  }

  case class Sum(e1: Expr, e2: Expr) extends Expr

  case class Prod(e1: Expr, e2: Expr) extends Expr

  val twoSum = Sum(Number(2), Number(3))
  val threeSum = Sum(Sum(Number(2), Number(3)), Number(4))
  val productSum = Prod(Sum(Number(2), Number(1)), Number(3))
  val sumProduct = Sum(Prod(Number(2), Number(1)), Number(3))
  val mystery = Prod(Sum(Number(2), Number(3)), Sum(Number(3), Number(4)))

  def translate(expr: Expr): String =
    expr match {
      case Number(n) => s"$n"
      case Sum(a, b) => s"${translate(a)} + ${translate(b)}"
      case Prod(a, b) => {
        def maybeShowParentheses(e: Expr) = e match {
          case Sum(_,_) => "(" + translate(e) + ")"
          case _ => translate(e)
        }

        maybeShowParentheses(a) + " * " + maybeShowParentheses(b)
      }
      case _ => "This case isn't handled yet"
    }

  println(translate(twoSum))
  println(translate(threeSum))
  println(translate(productSum))
  println(translate(sumProduct))
  println(translate(mystery))
}
