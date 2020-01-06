package exercises.redux

abstract class MeList {
  def head: Int
  def tail: MeList
  def isEmpty: Boolean
  def add(i: Int): MeList
  def printElements: String

  override def toString: String = {
    "[" + printElements + "]"
  }
}

object Empty extends MeList {
  override def head: Int = throw new NoSuchElementException
  override def tail: MeList = throw new NoSuchElementException
  override def isEmpty: Boolean = true
  override def add(i: Int): MeList = new Cons(i, Empty)
  override def printElements = ""
}

class Cons(h: Int, t: MeList) extends MeList {
  override def head: Int = h
  override def tail: MeList = t
  override def isEmpty: Boolean = false
  override def add(i: Int): MeList = new Cons(i, this)
  override def printElements =
    if (t.isEmpty) "" + h
    else h + " " + t.printElements
}

object Demo extends App {
  val empty = Empty
  println(empty)

  val one: MeList = Empty.add(1)
  val two = one.add(2)
  val three = two.add(3)

  println(three)
}
