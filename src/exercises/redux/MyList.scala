package exercises.redux

trait MyPredicate[-T] {
  def test(value: T): Boolean
}

trait MyTransformer[-A, B] {
  def transform(input: A): B
}

abstract class MeList[+A] {
  def head: A
  def tail: MeList[A]
  def isEmpty: Boolean
  def add[B >: A](i: B): MeList[B]
  def printElements: String

  override def toString: String = {
    "[" + printElements + "]"
  }

  def map[B](transformer: MyTransformer[A, B]): MeList[B]
  def flatMap[B](transformer: MyTransformer[A, MeList[B]]): MeList[B]
  def filter(predicate: MyPredicate[A]): MeList[A]
  def ++[B >: A](list: MeList[B]): MeList[B]
}

case object Empty extends MeList[Nothing] {
  override def head: Nothing = throw new NoSuchElementException
  override def tail: MeList[Nothing] = throw new NoSuchElementException
  override def isEmpty: Boolean = true
  override def add[B >: Nothing](i: B): MeList[B] = new Cons(i, Empty)
  override def printElements = ""

  def map[B](transformer: MyTransformer[Nothing, B]) = Empty
  def flatMap[B](transformer: MyTransformer[Nothing, MeList[B]]): MeList[B] = Empty
  def filter(predicate: MyPredicate[Nothing]): MeList[Nothing] = Empty
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

  override def map[B](transformer: MyTransformer[A, B]): MeList[B] = {
    new Cons(transformer.transform(head), tail.map(transformer))
  }

  override def flatMap[B](transformer: MyTransformer[A, MeList[B]]): MeList[B] =
    transformer.transform(head) ++ t.flatMap(transformer)

  override def filter(predicate: MyPredicate[A]): MeList[A] = {
    if (predicate.test(head)) new Cons(h, t.filter(predicate))
    else t.filter(predicate)
  }
  override def ++[B >: A](list: MeList[B]): MeList[B] = new Cons(h, t ++ list)
}

object Demo extends App {
  val listOfIntegers: MeList[Int] = new Cons(1, new Cons(8, new Cons(17, Empty)))
  val cloneListOfIntegers: MeList[Int] = new Cons(1, new Cons(8, new Cons(17, Empty)))
  val secondListOfIntegers: MeList[Int] = new Cons(10, new Cons(80, new Cons(170, Empty)))
  val listOfStrings: MeList[String] = new Cons("Tick", new Cons("Arthur", new Cons("Dot", new Cons("Overkill", Empty))))

  println(listOfIntegers.toString)
  println(listOfStrings.toString)

  println(listOfIntegers.map(new MyTransformer[Int, Int] {
    override def transform(elem: Int): Int = {
      4 * elem
    }
  }))

  println(listOfIntegers.filter(new MyPredicate[Int] {
    override def test(elem: Int): Boolean = {
      elem > 5
    }
  }))

  println(listOfIntegers ++ secondListOfIntegers)
  println(listOfIntegers.flatMap(new MyTransformer[Int, MeList[Int]] {
    override def transform(elem: Int): MeList[Int] = new Cons(elem, new Cons(elem + 1, Empty))
  }))

  println(listOfStrings.map(new MyTransformer[String, String] {
    override def transform(elem: String): String = {
      s"$elem is a superhero! \n"
    }
  }))

  println(listOfIntegers == cloneListOfIntegers)
}