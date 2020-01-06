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
}

object Empty extends MeList[Nothing] {
  override def head: Nothing = throw new NoSuchElementException
  override def tail: MeList[Nothing] = throw new NoSuchElementException
  override def isEmpty: Boolean = true
  override def add[B >: Nothing](i: B): MeList[B] = new Cons(i, Empty)
  override def printElements = ""
}

class Cons[+A](h: A, t: MeList[A]) extends MeList[A] {
  override def head: A = h
  override def tail: MeList[A] = t
  override def isEmpty: Boolean = false
  override def add[B >: A](i: B): MeList[B] = new Cons[B](i, this)
  override def printElements =
    if (t.isEmpty) "" + h
    else h + " " + t.printElements
}

object Demo extends App {
  val empty = Empty
  println(empty)

  val one: MeList[Double] = Empty.add(6.419)
  val two = one.add(3.14258)
  val three = two.add(1)

  println(three)
}
