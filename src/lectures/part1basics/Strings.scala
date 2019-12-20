package lectures.part1basics

object Strings extends App {
  val str = "Hello Dolly, well Hello Dolly"

  println(str.charAt(2))
  println(str.substring(7, 11))
  println(str.split(" ").toList)
  println(str.startsWith("Hello"))
  println(str.replace(" ", "-"))
  println(str.toLowerCase)
  println(str.toUpperCase)
  println(str.length)

  val numberString = "45"
  val number = numberString.toInt
  println('a' +: numberString :+ 'z')
  println(str.reverse)
  println(str.reverse)
  println(str.take(2))

  // Scala specific interpolation notation
  val name = "David"
  val age = 12
  val greeting = s"Hello, my name is $name and I will be turning ${age + 1} years old"
  println(greeting)

  // F-Interpolator
  val speed = 1.2f
  val myth = f"$name can eat $speed%2.2f burgers per minute"
  println(myth)

  //raw interpolated
  println(raw"This is a \n newline")
  val escape = "This is a \n newline"
  println(raw"$escape")
}
