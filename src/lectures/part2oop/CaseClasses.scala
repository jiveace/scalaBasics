package lectures.part2oop

object CaseClasses extends App{

  // Case classes good for storing data fields as opposed to logic
  case class Person(name: String, age: Int)

  // 1. class parameters are fields
  val jemima = new Person("Jemima", 82)
  println(jemima.name)


  // 2. sensible toString
  println(jemima)

  // 3. equals and hashCode implemented out of box
  println(jemima.hashCode())

  val jemima2 = new Person("Jemima", 82)
  println(jemima == jemima2)

  // 4. CCs have handy copy methods
  val jemima3 = jemima2.copy(age = 75)
  println(jemima3)

  // 5. CCs have companion objects
  val thePerson = Person
  val jane = Person("Jane", 23)
  println(thePerson)

  // 6. CCs are serializable
  //Useful in Akka framework

  // 7. CCs have extractor patterns -> They can be used in pattern matching

  case object UK {
    def name: String = "A nation divided"
  }

}
