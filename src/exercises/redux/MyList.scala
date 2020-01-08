package exercises.redux

abstract class MeList[+A] {
  def head: A

  def tail: MeList[A]

  def isEmpty: Boolean

  def add[B >: A](i: B): MeList[B]

  def printElements: String

  override def toString: String = {
    "[" + printElements + "]"
  }

  def map[B](transformer: A => B): MeList[B]

  def flatMap[B](transformer: A => MeList[B]): MeList[B]

  def filter(predicate: A => Boolean): MeList[A]

  def ++[B >: A](list: MeList[B]): MeList[B]
}

case object Empty extends MeList[Nothing] {
  override def head: Nothing = throw new NoSuchElementException

  override def tail: MeList[Nothing] = throw new NoSuchElementException

  override def isEmpty: Boolean = true

  override def add[B >: Nothing](i: B): MeList[B] = new Cons(i, Empty)

  override def printElements = ""

  def map[B](transformer: Nothing => B) = Empty

  def flatMap[B](transformer: Nothing => MeList[B]): MeList[B] = Empty

  def filter(predicate: Nothing => Boolean): MeList[Nothing] = Empty

  override def ++[B >: Nothing](list: MeList[B]): MeList[B] = list
}

case class Cons[+A](h: A, t: MeList[A]) extends MeList[A] {
  override def head: A = h

  override def tail: MeList[A] = t

  override def isEmpty: Boolean = false

  override def add[B >: A](i: B): MeList[B] = new Cons[B](i, this)

  override def printElements =
    if (t.isEmpty) "" + h
    else h + " " + t.printElements

  override def map[B](transform: A => B): MeList[B] = {
    new Cons(transform(head), tail.map(transform))
  }

  override def flatMap[B](transform: A => MeList[B]): MeList[B] =
    transform(head) ++ t.flatMap(transform)

  override def filter(test: A => Boolean): MeList[A] = {
    if (test(head)) new Cons(h, t.filter(test))
    else t.filter(test)
  }

  override def ++[B >: A](list: MeList[B]): MeList[B] = new Cons(h, t ++ list)
}

object Demo extends App {
  val listOfIntegers: MeList[Int] = Cons(1, Cons(8, Cons(17, Empty)))
  val cloneListOfIntegers: MeList[Int] = Cons(1, Cons(8, Cons(17, Empty)))
  val secondListOfIntegers: MeList[Int] = Cons(10, Cons(80, Cons(170, Empty)))
  val listOfStrings: MeList[String] = Cons("Tick", Cons("Arthur", Cons("Dot", Cons("Overkill", Empty))))

  println(listOfIntegers.toString)
  println(listOfStrings.toString)

  println(listOfIntegers.map(4 * _))
  println(listOfIntegers.filter(_ < 5))

  println(listOfIntegers ++ secondListOfIntegers)
  println(listOfIntegers.flatMap(elem => new Cons(elem, new Cons(elem + 1, Empty))))


  println(listOfStrings.map(elem => s"$elem is a superhero! \n"))
  println(listOfIntegers == cloneListOfIntegers)
}