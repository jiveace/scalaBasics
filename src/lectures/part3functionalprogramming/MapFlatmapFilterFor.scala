package lectures.part3functionalprogramming

object MapFlatmapFilterFor extends App {

  val list = List(1, 2, 3)
  println(list.head)
  println(list.tail)

  //map
  println(list.map(_ + 1))
  println(list.map(_ + " is a number"))

  // filter
  println(list.filter(_ % 2 == 0))

  // flatmap
  val toPair = (x: Int) => List(x, x + " is a number")
  println(list.flatMap(toPair))

  // print all combos betwen two lists
  val colours = List("Red", "Black", "Yellow")
  val characters = List("Dot", "Tick", "Kane", "Paul")
  val powers = List("Flying", "Invisible", "Superstrong")

  // "iterating"
  val result = colours.flatMap(n => characters.flatMap(c => powers.map(p => p + " " + n + " " + c)))
  println(result)

  //foreach
  //  result.foreach(println)

  // for-comprehension
  val forCombinations = for {
    c <- colours
    ch <- characters
    p <- powers if p.length > 7
  } yield p + " " + ch + " " + c

  println(forCombinations)

  for {
    p <- powers
  } println(p)


}
