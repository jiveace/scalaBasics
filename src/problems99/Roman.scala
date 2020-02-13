package problems99

object Roman {

  val I = "I"
  val V = "V"
  val X = "X"
  val L = "L"
  val C = "C"
  val D = "D"
  val M = "M"

  private def mapDigit(str: String): Int = str match {
    case I => 1
    case V => 5
    case X => 10
    case L => 50
    case C => 100
    case D => 500
    case M => 1000
    case _ => throw new IllegalStateException("Unsupported denomination")
  }

  private def validate(str: String): String = {
    val splitString = str.split("").toList

    def areSubtractivePair(a: String, b: String): Boolean = (a, b) match {
      case (I, V) => true
      case (I, X) => true
      case (X, L) => true
      case (X, C) => true
      case (C, D) => true
      case (C, M) => true
      case _ => throw new IllegalStateException(s"$a$b is an invalid numeral in this context")
    }

    def validateNumeralOrder(list: List[String], lastNumeralIsBig: Boolean):Unit = list match {
      case _ :: Nil => ()
      case head :: tail if mapDigit (head) >= mapDigit (tail.head) || areSubtractivePair (head, tail.head ) && lastNumeralIsBig =>
        validateNumeralOrder (tail, mapDigit (head) > mapDigit (tail.head ))
      case _ => throw new IllegalStateException ("Numerals are not provided in descending order")
    }

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

  def toInt(romans: String) = {
    def _toInt(list: List[Int], total: Int): Int = list match {
      case head :: Nil => total + head
      case head :: tail if head < tail.head => _toInt(tail, total - head)
      case head :: tail => _toInt(tail, total + head)
    }

    if (romans == null || romans.isEmpty) 0
    else _toInt(validate(romans).split("").map(mapDigit(_)).toList, 0)
  }
}
