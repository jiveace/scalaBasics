package lectures.part2oop

object Genrics extends App {

  class MyList[+A] {
    // use the type A
    def add[B >: A](element: B): MyList[B] = ???
  }

  class MyMap[Key, Value]

  val listOfIntegers = new MyList[Int]
  val listOfStrings = new MyList[String]

  //  generic methods
  object MyList {
    //    def empty[A]: MyList[A] = ???
  }

  //  val emptyLIstOfIntegers = MyList.empty[Int]

  // variance problem
  class Animal

  class Cat extends Animal

  class Dog extends Animal

  // List[Cat] extends List[Animals]
  class CovariantList[+A]

  val animal: Animal = new Cat
  val animalList: CovariantList[Animal] = new CovariantList[Cat]

  //animalList.add(new Dog) ??? HARD QUESTION
  // We return a list of animals

  // 2. NO - INVARIANCE
  class InvariantList[+A]

  val invariantAnimalList: InvariantList[Animal] = new InvariantList[Dog]

  //3. Hell no!  CONTRAVARIANCE
  class Trainer[-A]

  val trainer: Trainer[Cat] = new Trainer[Animal]


  // bounded types
  // Upper Bounded Type - Type A must be a subtype of Animal, or Animal
  class Cage[A <: Animal](animal: A)

  // Lower Bounded Type - Type A must be a supertype of Animal, or Animal in theory - disnae wurk
  //  class Cage[A >: Animal](animal: A)
  val cage = new Cage(new Dog)

  class Car

  val car = new Cage(new Animal)
}
