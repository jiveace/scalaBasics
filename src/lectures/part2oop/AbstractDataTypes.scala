package lectures.part2oop

object AbstractDataTypes extends App {

  abstract class Animal {
    // Lack of value of RHS is equivalent of abstract
    val creatureType: String

    def eat: Unit
  }

  class Dog extends Animal {
    override val creatureType: String = "Amazing"
    override def eat = println("Eating ravenously")
  }

  // traits
  trait Carnivore {
    def eat(animal: Animal): Unit
  }

  trait ColdBlooded

  class Crocodile extends Animal with Carnivore with ColdBlooded {
    val creatureType: String = "terrifying"
    def eat = println("Snappy snappy")

    override def eat(animal: Animal): Unit = println(s"Yum I am eating a ${animal.creatureType} animal")
  }

  val croc = new Crocodile
  croc.eat(new Dog())

  // traits vs abstract classes
  // 1 - traits do not have constructor parameters
  // 2 - multiple traits may be inherited by the same class
  // 3 - traits = behaviour (verb), absrtact class = thing (noun)
}
