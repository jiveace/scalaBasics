package lectures.part3functionalprogramming

import scala.util.Random

object Options extends App {
  val myFirstOption: Option[Int] = Some(4)
  val noOption: Option[Int] = None

  // Unsafe APIs
  def unsafeMethod(): String = null

  //  def result = Some(unsafeMethod()) // Wrong - unsafe (Some(null) is possible)
  val result = Option(unsafeMethod());
  println(result)

  // chained methods
  def safeMethod(): String = "Hooray for Naito"

  val chainedResult = Option(unsafeMethod()).orElse(Option(safeMethod()))
  println(chainedResult)

  //DESIGN unsafe APIs
  def betterUnsafeMethod(): Option[String] = None

  def betterSafeMethod(): Option[String] = Some("A valid result")

  val betterChainedResult = betterUnsafeMethod() orElse betterSafeMethod()

  //  functions on options
  println(myFirstOption.isEmpty)
  println(myFirstOption.get) // UNSAFE - DO NOT USE

  // map, flatmap, filter
  println(myFirstOption.map(_ * 2))
  println(myFirstOption.filter(_ == 7))
  println(myFirstOption.flatMap(x => Option(x * 10)))

  // for-comprehensions

  /*
   Quizzes
   */
  val config: Map[String, String] = Map(
    "host" -> "176.145.36.1",
    "port" -> "80"
  )

  class Connection {
    def connect = "Connected" //stub
  }

  object Connection {
    val random = new Random(System.nanoTime())

    def apply(host: String, port: String): Option[Connection] =
      if (random.nextBoolean()) Some(new Connection)
      else None
  }

  // try to establish a connection
  // If so, print connection
  // If fails, print error
  val hostOption = config.get("host")
  val portOption = config.get("port")
  val connection: Option[Connection] = hostOption.flatMap(h => portOption.flatMap(p =>Connection.apply(h, p)))
  val connectionStatus = connection.map(c => c.connect)
  println(connectionStatus)


  def flip(x: Int): Boolean =
    new Random().nextBoolean()

  val runs = 1_000_000
  val heads = (1 to runs).map(flip).count(_==true)
  val tails = runs - heads
  println(s"heads: $heads Tails: $tails")
  println((1 to 100).map(flip))
}
