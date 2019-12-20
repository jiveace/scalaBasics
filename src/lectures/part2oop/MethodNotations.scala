package lectures.part2oop

import scala.language.postfixOps

object MethodNotations extends App {

  class Person(val name: String, favouriteMovie: String = "Ghostbusters", val age: Int = 0) {
    def likes(movie: String): Boolean = movie == favouriteMovie

    def danceWith(person: Person): String = s"${name} is dancing with ${person.name}"

    def -->(person: Person): String = s"${name} implies ${person.name}"

    def apply(): String = s"${name}'s apply() method"

    def +(nickname: String) = new Person(name = s"${this.name} ($nickname)")

    def unary_+ : Person = new Person(name, favouriteMovie, age + 1)

    def learnsBalboa: String = learns("Balboa")

    def learnsBlues: String = learns("Blues")

    def learns(subject: String): String = s"${name} learns $subject"

    def apply(times: Int) = s"${name} watched $favouriteMovie $times times"

  }

  val mary = new Person("Mary")
  println(mary likes "Ghostbusters")
  // infix notation

  // "operators" in Scala
  val jane: Person = new Person("Jane")
  println(mary danceWith jane)
  println(mary danceWith new Person("Alex", "The Dark Knight"))

  // '-->' is a valid method name :-S
  println(mary --> mary)

  //ALL OPERATORS ARE METHODS!

  println(mary.apply)
  println(jane())


  // QUIZZES
  /*
  Overload +
  mary + "the genie" => new Person "Mary (the Genie)"
  */
  println((mary + "The Genie").name)

  /*
  Add an age to the person class
  Add a unary+ operator => new Person with age++
  +mary -> new mary
  */
  println(mary.age)
  println((+mary).age)

  /*
  Add 'learns' method to person -> Mary Learns Scala
  All learns scala method -> calls learns with scala as parameter
  use in postfix
  */
  println(jane learnsBalboa)
  println(jane.learnsBlues)
  println(jane learns ("Lindy"))

  /*
  overload apply method
  mary.apply(2) -> mary watched <movie> n times
  */
  println(mary(7))
  println(new Person("Alex", "The Dark Knight")(5))
}

