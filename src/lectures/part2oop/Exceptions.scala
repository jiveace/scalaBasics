package lectures.part2oop

object Exceptions extends App{

  val x: String = null

  // Throw 'em
//  val anOddValue: String = throw new IndexOutOfBoundsException

  // Catch 'em
  def getInt(withExceptions: Boolean): Int =
    if (withExceptions) throw new RuntimeException("No int for you!")
    else 42

  val potentialFail = try {
    getInt(true)
  } catch {
    case e: RuntimeException => 46
  } finally {
    // Optional, of course, but does not influence the return value of the expression
    // Used only for side effects
    println("Finally!")
    50
  }

  println(potentialFail)


  // Define your own exceptions

  class SachinException extends Exception

  throw new SachinException()


  // Not doing exercise until I get the test framework working - enough is enough
}
