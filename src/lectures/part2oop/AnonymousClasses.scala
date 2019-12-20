package lectures.part2oop

object AnonymousClasses extends App {

  abstract class Animal {
    def eat: Unit
  }

  val funnyAnimal: Animal = new Animal {
    override def eat: Unit = println("Plop")
  }

  println(funnyAnimal.eat)

  class Person(name: String) {
    def sayHi: Unit = println(name)
  }

  val jim = new Person("Jim") {
    override def sayHi: Unit = println("I'm Jimmy!")
  }

  /*
    * Generic trait  MyPredicate[T]
    * Generic trait MyTransformer[A, B]
    * MyList:
    *  - map(myTransformer) => MyList
    *  - filter(predicate) => MyList
    *  - flatMap(transformer from A to MyList[B] => MyList[B])
    */
}
