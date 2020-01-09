package lectures.part3functionalprogramming

import scala.util.Random

object Sequences extends App {

  val aSequence = Seq(1, 2, 3, 4)
  println(aSequence)
  println(aSequence.reverse)
  println(aSequence(2))
  println(aSequence ++ Seq(7, 6, 5))
  println(aSequence.sorted)

  val aRange: Seq[Int] = 1 until 10
  aRange.foreach(println)

  (1 to 10).foreach(x => println("Hello"))

  // lists
  val aList = List(2, 3, 4)
  val prepended = 42 +: aList :+ 98
  println(prepended)

  val apples5 = List.fill(5)("apple")
  println(apples5)
  println(aList.mkString("-|-"))

  // arrays
  val numbers = Array(1, 2, 3, 4)
  val treeElements = Array.ofDim[String](3)
  treeElements.foreach(println)

  //mutation
  // Sugar for numbers.update(2,0)
  numbers(2) = 0
  println(numbers.mkString(" "))

  // arrays and seq
  val numbersSeq: Seq[Int] = numbers // implicit conversion
  println(numbersSeq)

  // Vectors
  val vector: Vector[Int] = Vector(1,2,3)
  vector.foreach(println)

  // Vectors vs Lists
  val maxRuns = 1000
  val maxCapacity = 1000000

  def getWriteTime(collection: Seq[Int]): Double = {
    val r = new Random
    val times = for {
      it <- 1 to maxRuns
    } yield {
      val currentTime = System.nanoTime()
      collection.updated(r.nextInt(maxCapacity), 0)
      System.nanoTime() - currentTime
    }

    times.sum * 1.0 / maxRuns
  }

  val numbersList = (1 to maxCapacity).toList
  val numbersVector = (1 to maxCapacity).toVector

  // :-) keeps reference to tail
  // :-( updating an element in middle takes a long time
  println(getWriteTime(numbersList))
  // :-) depth of the tree is small
  // :-( needs to replace an entire 32 element chunk
  println(getWriteTime(numbersVector))
}
