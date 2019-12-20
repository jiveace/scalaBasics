package lectures.part1basics

object Expressions extends App {
  var x = 1 + 2
  println(x)

  println(2 + 3 * 4)

  var variable = 2
  variable += 3
  println(variable)

  // Right hand side is an IF EXPRESSION
  val ternaryEquivalent = if(variable==3) "Three" else "Not Three"
  println(ternaryEquivalent)

  val weirdness = (x = 67)
  println(weirdness)
  println(x)

  val aCodeBlock = {
    val y = 2
    val z = y + 1

    // Value of last statement is returned, and assigned to val
    if (z > 2) "hello" else "goodbye"
  }

  println(aCodeBlock)

  //Questions
  // 1. Difference betwixt String "hello world" vs println("Hello World")?
  // First is an expression with value "Hello World", second is an instruction with the value 'Unit'

  // 2.
  val someValue = {
    2 < 3
  }
  // Code block returns the value of the last statement and applies it to the val.  So true.

  // 3.
  val someOtherValue = {
    if (someValue) 239 else 980
    42
  }
  // Code block returns the value of the last statement and applies it to the val.  So 42.
  println(someOtherValue)
}
