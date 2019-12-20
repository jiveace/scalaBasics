package lectures.part2oop

object Objects {

  def main(args: Array[String]): Unit = {
    // SCALA DOES NOT HAVE CLASS LEVEL FUNCTIONALITY ("static")
    object Person {
      // static functionality
      val N_EYES = 2;

      def canFly: Boolean = false

      // Factory method - 'static' method that produces instances
      def apply(mother: Person, father: Person): Person = {
        new Person("Owen")
      }
    }
    class Person(val name: String) {
      // instance level functionality
    }
    // COMPANIONS - same scope, same name

    println(Person.N_EYES)
    println(Person.canFly)

    // Scala object = SINGLETON INSTANCE

    val mary = Person
    val john = Person
    println(mary == john)

    val jane = new Person("Jane")
    val alex = new Person("Alex")
    println(jane == alex)

    val owen = Person(mother = jane, alex)
    println(owen.name)

    // Scala Applications = Scala object with method
    // def main(args: Array[String]): Unit

  }
}
