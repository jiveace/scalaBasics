package exercises

trait MyPredicate[-T] {
  def test(input: T): Boolean
}

trait MyTransformer[-A, B] {
  def transform(input: A): B
}

abstract class MyList[+A] {
  def head: A
  def tail: MyList[A]
  def isEmpty: Boolean
  def add[B >: A](i: B): MyList[B]
  def printElements: String

  override def toString: String = {
    "[" + printElements + "]"
  }

  def map[B](transformer: MyTransformer[A, B]): MyList[B]
  def flatMap[B](transformer: MyTransformer[A, MyList[B]]): MyList[B]
  def filter(predicate: MyPredicate[A]): MyList[A]

  // Concatenation
  def ++[B >: A](list: MyList[B]): MyList[B]
}

object Empty extends MyList[Nothing] {
  override def head: Nothing = throw new NoSuchElementException
  override def tail: Nothing = throw new NoSuchElementException
  override def add[B >: Nothing](i: B): MyList[B] = new Cons(i, Empty)
  def isEmpty = true
  override def printElements: String = ""
  override def filter(predicate: MyPredicate[Nothing]): MyList[Nothing] = this
  override def map[B](transformer: MyTransformer[Nothing, B]): MyList[Nothing] = this
  override def flatMap[B](transformer: MyTransformer[Nothing, MyList[B]]): MyList[B] = this
  override def ++[B >: Nothing](list: MyList[B]): MyList[B] = this
}

class Cons[+A](h: A, t: MyList[A]) extends MyList[A] {
  override def head: A = h
  override def tail: MyList[A] = t
  override def add[B >: A](i: B): MyList[B] = new Cons(i, this)
  def isEmpty = head == false

  override def printElements: String =
    if (t.isEmpty) "" + h
    else h + " " + t.printElements

  override def filter(predicate: MyPredicate[A]): MyList[A] = {
    if (predicate.test(h)) new Cons(h, t.filter(predicate))
    else t.filter(predicate)
  }

  override def map[B](transformer: MyTransformer[A, B]): MyList[B] = {
    new Cons(transformer.transform(h), t.map(transformer))
  }

  def ++[B >: A](list: MyList[B]): MyList[B] = {
    new Cons(h, t ++ list)
  }

  override def flatMap[B](transformer: MyTransformer[A, MyList[B]]): MyList[B] = {
    transformer.transform(h) ++ t.flatMap(transformer)
  }
}

object Demo extends App {
  val listOfInts: MyList[Int] = new Cons(1, new Cons(2, new Cons(3, Empty)));
  val listOfInts2: MyList[Int] = new Cons(14, new Cons(15, new Cons(16, Empty)));
  val listOfStrings: MyList[String] = new Cons("Four", new Cons("Five", new Cons("Six", Empty)));

  println(listOfInts.toString)
  println(listOfStrings.toString)

  println(listOfInts.map(new MyTransformer[Int, Int] {
    override def transform(input: Int): Int = 3 * input
  }).toString)
  println(listOfInts ++ listOfInts2)
  println(listOfInts.flatMap(new MyTransformer[Int, MyList[Int]] {
    override def transform(input: Int): MyList[Int] = new Cons(input, new Cons(input + 1, Empty))
  }).toString)

}