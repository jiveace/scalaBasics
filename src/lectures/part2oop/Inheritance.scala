package lectures.part2oop

object Inheritance extends App {

  class Animal {
    val creatureType = "Alive"
    def eat = println("omnomnom")
  }

  class Cat extends Animal {
    def crunch = {
      eat
      println("Munchety Crunchety")
    }
  }

  val cat = new Cat
  cat.crunch


  //Constructors
  class Person(name: String, age: Int) {
    def this(name: String) = this(name, 0)
  }

  class Adult(name: String, age: Int, idCard: String) extends Person("Jeff")


  // Overriding
  class Dog(override val creatureType: String) extends Animal {
    override def eat = {
      super.eat
      println("crunch, crunch")
    }

//    override val creatureType: String = "Awesome"
  }

  val dog = new Dog("Awesome")
  dog.eat
  println(cat.creatureType)
  println(dog.creatureType)

  //type substitution
  val unknownAnimal: Animal = dog
  unknownAnimal.eat

  // super

  // preventing overrides
  // scala has final keyword
  // Also has 'sealed' - can extend the filed in THIS file, but not other files
}
