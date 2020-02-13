package problems99

object Roman {

  def toInt(romans: String) = {

    def mapDigit(str: String): Int = str match {
      case "I" => 1
      case "V" => 5
      case "X" => 10
      case _ => throw new IllegalStateException("Unsupported denomination")
    }

    def _validate(str: String): String = {
      def checkForTenConsecutive(n: String) =
        if (str.contains(n * 10))
          throw new IllegalStateException(s"Input includes 10 consecutive ${n}s")

      def checkForMultiplesOf(n: String): Unit =
        if (str.split("").count(_ == n) > 1)
          throw new IllegalStateException(s"Multiple ${n}s are not allowed")

      checkForMultiplesOf("V")
      checkForTenConsecutive("I")
      checkForTenConsecutive("X")
      str
    }

    def _toInt(list: Array[Int], total: Int): Int =
      if (list.size == 1) total + list.last
      else if (list(0) < list(1)) _toInt(list.tail, total - list(0))
      else _toInt(list.tail, total + list(0))

    if (romans == null || romans.isEmpty) 0
    else _toInt(_validate(romans).split("").map(mapDigit(_)), 0)
  }
}
