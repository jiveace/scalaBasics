package lectures.part1basics

object CBNvsCBV extends App {

  def calledByValue(x: Long): Unit = {
    println("By Value: " + x)
    println("By Value: " + x)
  }

  def calledByName(x: => Long): Unit = {
    println("By Name: " + x)
    println("By Name: " + x)
  }

  calledByValue(System.nanoTime())
  calledByName(System.nanoTime())

  def infinite(): Int = 1 + infinite()

  def printFirst(x: Int, y: => Int) = println(x)

  //Evaluates infinite - crashes JVM
  //  printFirst(infinite, 34);

  //Only Evaluates infinite when it is used, so this runs fine
  //Sadly, IDE doesn't report unused field :-(
  printFirst(34, infinite);
}
