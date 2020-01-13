package lectures.part4patternmatching

import exercises.redux.{Cons, Empty, MeList}

object AllThePatterns extends App {

  // Constants
  val x: Any = "Scala"
  val constants = x match {
    case 1 => "a number"
    case "Scala" => "THE Scala"
    case true => "THE Truth"
    case AllThePatterns => "A singleton object"
  }

  // 2 - Match Anything
  // 2.1 wildcard
  val matchAnything = x match {
    case _ =>
  }

  // 2.2 variable
  val matchVariable = x match {
    case something => s"I sound ${something}"
  }

  // 3 Tuples
  val aTuple = (1, 2)
  val matchTuple = aTuple match {
    case (1, 1) =>
    case (something, 2) => s"I sound ${something}"
  }

  val nestedTuple = (1, (2, 3))
  val matchNestedTuple = nestedTuple match {
    case (_, (2, v)) =>
  }
  // Pattern matches can be nested

  // 4 - case classes - constructor pattern
  val aList: MeList[Int] = Cons(1, Cons(2, Empty))
  val matchList = aList match {
    case Empty =>
    case Cons(h, Cons(subhead, subtail)) =>
    // Pattern matchers can be nested with case classes as well
  }

  // 5 - list patterns
  val aStandardList = List(1, 2, 3, 42)
  val standardListMatching = aStandardList match {
    case List(1, _, _, _) => //extractor - advanced
    case List(1, _*) => // vararg
    case 1 :: List(_) => //infix pattern
    case List(1, 2, 3) :+ 42 => // infix pattern
  }

  // 6 - type spceifiers
  val unknown: Any = 2
  val unknownMatch = unknown match {
    case list: List[Int] => // explicit type specifier
    case _ =>
  }

  // 7 - name binding
  val nameBindingMatch = aList match {
    case nonEmptyList@Cons(_, _) => // name binding makes you use the name later
    case Cons(1, rest@Cons(2, _)) => //
  }
  // 8 - MultiPatterns
//  val multiPattern = aList match {
//    case Empty | Cons(0, _) => //compound pattern or Multipattern
//  }

  // 9 - If Guards
  val secondElementSpecial = aList match {
    case Cons(_, Cons(specialElement, _)) if specialElement % 2 == 0 =>
  }

  // ALL

  /*
  Quizzes
   */

  val numbers = List(1.2,224.0,3.69)
  val numbersMatch = numbers match {
    case listOfNumbers: List[Int] => "A list of numbers"
    case listOfDoubles: List[Double] => "A list of doubles"
    case listOfStrings: List[String] => "A list of strings"
    case _ => ""
  }

  println(numbersMatch)
}
