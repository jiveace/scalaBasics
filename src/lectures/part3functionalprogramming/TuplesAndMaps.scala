package lectures.part3functionalprogramming

import scala.annotation.tailrec

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


  def add(network: Map[String, Set[String]], person: String): Map[String, Set[String]] =
    network + (person -> Set())

  def remove(network: Map[String, Set[String]], person: String): Map[String, Set[String]] = {
    def removeAux(friends: Set[String], networkAcc: Map[String, Set[String]]): Map[String, Set[String]] =
      if (friends.isEmpty) networkAcc
      else removeAux(friends.tail, unfriend(networkAcc, person, friends.head))

    val unfriended = removeAux(network(person), network)
    unfriended - person
  }

  def friend(network: Map[String, Set[String]], left: String, right: String) = {
    network +
      (left -> (network(left) + right)) +
      (right -> (network(right) + left))
  }

  def unfriend(network: Map[String, Set[String]], left: String, right: String) = {
    network +
      (left -> (network(left) - right)) +
      (right -> (network(right) - left))
  }

  val network: Map[String, Set[String]] = Map("Jane" -> Set("Bane", "Tanahashi"), "Bane" -> Set("Jane"), "Tex" -> Set(), "Tanahashi" -> Set("Jane"))
  //  println(friend(network, "Jane", "Bane"))
  //  println(unfriend(friend(network, "Jane", "Bane"), "Jane", "Bane"))
  println(network)

  def mostFriends(network: Map[String, Set[String]]): String =
    network.maxBy(pair => pair._2.size)._1

  def lonelyCount(network: Map[String, Set[String]]): Int =
    network.count(_._2.isEmpty)

  def socialConnection(network: Map[String, Set[String]], a: String, b: String): Boolean = {
    @tailrec
    def bfs(target: String, consideredPeople: Set[String], discoveredPeople: Set[String]): Boolean = {
      if (discoveredPeople.isEmpty) false
      else {
        val person = discoveredPeople.head
        if (person == target) true
        else if (consideredPeople.contains(person)) bfs(target, consideredPeople, discoveredPeople.tail)
        else bfs(target, consideredPeople + person, discoveredPeople.tail ++ network(person))
      }
    }

    bfs(b, Set(), network(a) + a)
  }

  println(mostFriends(network))
  println(lonelyCount(network))
  println(socialConnection(network, "Tanahashi", "Bane"))
}
