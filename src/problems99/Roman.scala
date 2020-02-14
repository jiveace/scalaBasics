package problems99

object Roman {

  val I = "I"
  val V = "V"
  val X = "X"
  val L = "L"
  val C = "C"
  val D = "D"
  val M = "M"
  val BLANK = ""

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

  private def areSubtractivePair(a: String, b: String): Boolean = (a, b) match {
    case (I, V) => true
    case (I, X) => true
    case (X, L) => true
    case (X, C) => true
    case (C, D) => true
    case (C, M) => true
    case _ => false
  }

  private def validate(str: String): String = {
    val splitString = str.split("").toList

    def validateNumeralOrder(list: List[String], lastNumeralIsBig: Boolean): Unit = list match {
      case _ :: Nil => ()
      case head :: tail if mapDigit(head) >= mapDigit(tail.head) || areSubtractivePair(head, tail.head) && lastNumeralIsBig =>
        validateNumeralOrder(tail, mapDigit(head) > mapDigit(tail.head))
      case _ => throw new IllegalStateException("Numerals are not provided in descending order")
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

  def toRoman(x: Int): String = {
    val numerals = List(
      (M, 1000),
      ("CM", 900),
      (D, 500),
      ("CD", 400),
      (C, 100),
      ("XC", 90),
      (L, 50),
      ("XL", 40),
      (X, 10),
      ("IX", 9),
      (V, 5),
      ("IV", 4),
      (I, 1))

    def _toRoman(remainder: Int, _numerals: List[(String, Int)], result: String): String = _numerals match {
      case Nil => result
      case head :: tail => _toRoman(remainder % head._2, tail, result + (head._1 * (remainder / head._2)))
    }

    if (x < 0) throw new IllegalArgumentException
    else _toRoman(x, numerals, "")
  }

  def toNumeralList(str: String): List[String] = {

    def _toNumeralList(remainder: List[String], result: List[String]): List[String] = remainder match {
      case Nil => result
      case h :: Nil => result :+ h
      case h :: t if areSubtractivePair(h, t.head) => _toNumeralList(t.tail, result :+ (h + t.head))
      case h :: t => _toNumeralList(t, result :+ h)
    }

    val list = str.split("").toList
    _toNumeralList(list, List())
  }
}
