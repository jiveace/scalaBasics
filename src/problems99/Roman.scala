package problems99

object Roman {

  val I = "I"
  val V = "V"
  val X = "X"
  val L = "L"
  val C = "C"
  val D = "D"
  val M = "M"

  def toInt(romans: String) = {

    def mapDigit(str: String): Int = str match {
      case I => 1
      case V => 5
      case X => 10
      case L => 50
      case C => 100
      case D => 500
      case M => 1000
      case _ => throw new IllegalStateException("Unsupported denomination")
    }

    def validate(str: String): String = {
      val splitString = str.split("")

      def areSubtractivePair(a: String, b: String): Boolean = (a, b) match {
        case ("I", "V") => true
        case ("I", "X") => true
        case ("X", "L") => true
        case ("X", "C") => true
        case ("C", "D") => true
        case ("C", "M") => true
        case _ => throw new IllegalStateException(s"$a$b is an invalid numeral in this context")
      }

      def validateNumeralOrder(list: Array[String], lastNumeralIsBig: Boolean): Unit =
        if (list.size == 1) ()
        else if (mapDigit(list(0)) >= mapDigit(list(1)) || (areSubtractivePair(list(0), list(1))) && lastNumeralIsBig) validateNumeralOrder(list.tail, mapDigit(list(0)) > mapDigit(list(1)))
        else throw new IllegalStateException("Numerals are not provided in descending order")

      def checkForTenConsecutive(n: String) =
        if (str.contains(n * 10))
          throw new IllegalStateException(s"Input includes 10 consecutive ${n}s")

      def checkForMultiplesOf(n: String): Unit =
        if (str.split("").count(_ == n) > 1)
          throw new IllegalStateException(s"Multiple ${n}s are not allowed")

      validateNumeralOrder(splitString, true)
      checkForMultiplesOf(V)
      checkForMultiplesOf(L)
      checkForMultiplesOf(D)
      checkForTenConsecutive(I)
      checkForTenConsecutive(X)
      checkForTenConsecutive(C)
      str
    }

    def _toInt(list: Array[Int], total: Int): Int =
      if (list.size == 1) total + list.last
      else if (list(0) < list(1)) _toInt(list.tail, total - list(0))
      else _toInt(list.tail, total + list(0))

    if (romans == null || romans.isEmpty) 0
    else _toInt(validate(romans).split("").map(mapDigit(_)), 0)
  }
}
