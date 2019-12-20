package lectures.part2oop

object OOBasics extends App {
  val jim = new Person("Jim", 46)
  println(jim.age)
  jim.greet("Enoch")
}

class Person(name: String, val age: Int) {
  // Effectively public
  val x = 2

  println(s"Constructing Person '$name'")

  def greet(name: String) = println(s"${this.name} says 'Hi $name'")

  // Auxhilliary Constructor
  def this(name: String) = this(name, 0)

  // Would be easier to provide a default param value in the main constructor

  val author = new Writer("Charles", "Dickens", 1812)
  val novel = new Novel("A Christmas Carol", 1861, author)

  println(novel.authorAge())
  println(novel.isWrittenBy(author))

  val counter = new Counter();
  counter.increment.increment.increment.print
  counter.decrement(3).print

  /*
    Novel and Writer
   */
  class Writer(firstName: String, surname: String, val year: Int) {
    def fullName() = s"$firstName $surname"
  }

  class Novel(name: String, yearOfRelease: Int, author: Writer) {
    def authorAge() = yearOfRelease - author.year

    def isWrittenBy(author: Writer) = this.author == author;

    def copy(newYearOfRelease: Int) = new Novel(name, newYearOfRelease, author)
  }

  class Counter(value: Int = 0) {
    def currentCount = value;

    def increment() = {
      println("Incrementing")
      new Counter(value + 1)
    }

    def decrement() = {
      println("Decrementing")
      new Counter(value - 1)
    }

    def increment(amount: Int): Counter = {
      if (amount <= 0) this
      else increment.increment(amount - 1)
    }

    def decrement(amount: Int): Counter = {
      if (amount <= 0) this
      else decrement.decrement(amount - 1)
    }

    def print() = println(currentCount)
  }

}
