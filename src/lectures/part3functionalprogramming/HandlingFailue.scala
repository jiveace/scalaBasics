package lectures.part3functionalprogramming

import scala.util.{Failure, Random, Success, Try}

object HandlingFailue extends App {

  // create success and failure
  val aSuccess: Success[Int] = Success(3)
  val aFailure: Failure[Nothing] = Failure(new RuntimeException("LEX EXPRESS"))

  println(aSuccess)
  println(aFailure)

  def unsafeMethod(): String = throw new RuntimeException("NO STRING FOR YOU BUSTER")

  // Try objects via the apply method
  val potentialFailure = Try(unsafeMethod())
  println(potentialFailure)

  // sugar
  val anotherPotentialFailure = Try(
    unsafeMethod()
  )
  println(anotherPotentialFailure)


  // utilities
  println(potentialFailure.isSuccess)
  println(potentialFailure.isFailure)

  def backupMethod() = "A value"

  val fallbackTry = Try(unsafeMethod()).orElse(Try(backupMethod()))
  println(fallbackTry)

  // map, flatMap, filter
  println(aSuccess.map(_ * 2))
  println(aSuccess.flatMap(x => Success(x * 10)))
  println(aSuccess.filter(_ > 10))

  /*
   * Exercise
   */
  val host = "localhost"
  val port = "8080"

  def renderHTML(page: String) = println(page)

  class Connection {
    def get(url: String): String = {
      val random = new Random(System.nanoTime())
      if (random.nextBoolean()) "<html></html>"
      else throw new RuntimeException("Connection Interrupted")
    }

    def getSafe(url: String): Try[String] = Try(get(url))
  }

  object HttpService {
    val random = new Random(System.nanoTime())

    def getConnection(host: String, port: String): Connection = {
      if (random.nextBoolean()) new Connection
      else throw new RuntimeException("Port already in use")
    }

    def getSafeConnection(host: String, port: String): Try[Connection] = Try(getConnection(host, port))
  }

  val possibleConnection = HttpService.getSafeConnection(host, port)
  val possibleHTML = possibleConnection.flatMap(connection => connection.getSafe("google"))
  possibleHTML.foreach(renderHTML)

  // chaining the calls
  HttpService.getSafeConnection(host, port)
    .flatMap(connection => connection.getSafe("google"))
    .foreach(renderHTML)

  // for-comprehension
  for {
    connection <- HttpService.getSafeConnection(host, port)
    html <- connection.getSafe("Google")
  } renderHTML(html)
}