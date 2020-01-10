package lectures.part3functionalprogramming

object TuplesAndMaps extends App {
  // tuples - finite ordered "lists
  val aTuple = new Tuple2(2, "HELLO!") //Tuple2[Int, String] = (Int, String)
  val aTuple2 = Tuple2(2, "HELLO!") //Tuple2[Int, String] = (Int, String)
  val aTuple3 = (2, "HELLO!") //Tuple2[Int, String] = (Int, String)

  println(aTuple._1)
  println(aTuple.copy(_2 = "WOO Nature Boy!"))
  println(aTuple.swap)


  // Maps - you know maps
  val aMap: Map[String, Int] = Map()

  val phoneBook: Map[String, String] = Map(("Jim", "6345789"), "Daniel" -> "8675309")
  println(phoneBook)

  // map ops
  println(phoneBook.contains("Jim"))
  println(phoneBook("Jim"))

  // add a pairing
  val newPairing = "Mary" -> "5555555"
  val newPhoneBook = phoneBook + newPairing
  println(newPhoneBook)

  // functionals on maps
  // map, flatMap, filter
  println(phoneBook.map(pair => pair._1.toUpperCase -> pair._2))

  // filterKeys - DISNAE WORK
  println(phoneBook.filterKeys(x => x.startsWith("J")))
  // mapValues - DEPRECTED METHODS NO WORKY
  println(phoneBook.mapValues(number => "0131" + number))

  println(phoneBook.toList)
  println(List("Dave" -> 42).toMap)

  // Very Cool
  val names = List("Bob", "James", "Anglea", "Mary", "Dan", "Jim")
  println(names.groupBy(name => name.charAt(0)))

  val duplicateMap = Map("JIM" -> "Second", "Jim" -> "Third")
  println(duplicateMap.map(pair => pair._1.toUpperCase -> pair._2))
}
