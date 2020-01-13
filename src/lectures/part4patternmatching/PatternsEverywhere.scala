package lectures.part4patternmatching

object PatternsEverywhere extends App {

  // big idea #1
  try {
    //code
  } catch {
    case e: RuntimeException => "Runtime"
    case npe: NullPointerException => "npe"
    case _ => "something else"
  }

  // catches are matches

  // big idea #2
  var list = List(1, 2, 3, 4)
  val evenOnes = for {
    x <- list if x % 2 == 0
  } yield x

  // generators are also based on PATTERN MATCHING
  val tuples = List((1, 2), (3, 4))
  val filterTuples = for {
    (first, second) <- tuples
  } yield first * second

  // big idea #3
  val tuple = (1, 2, 3)
  val (a, b, c) = tuple
  // Tuple destructuring!!

  val head :: tail = list

  // Big Idea #4
  // partial function
  val mappedList = list.map {
    case v if v % 2 == 0 => v + "is even"
    case 1 => "The One! Billy Gunn"
    case _ => "something else"
  }

  val mappedList2 = list.map { x =>
    x match {
      case v if v % 2 == 0 => v + "is even"
      case 1 => "The One! Billy Gunn"
      case _ => "something else"
    }
  }
}
