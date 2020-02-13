package problems99

object Roman {

  def toInt(romans: String) = {
    def mapDigit(str: String): Int = str match {
      case "I" => 1
      case "V" => 5
      case "X" => 10
      case _ => throw new IllegalStateException("Unsupported denomination")
    }

    def _validate(str: String): String =
      if (str.split("").count(_ == "V") > 1)
        throw new IllegalStateException("Too many Vs")
      else if (str.contains("IIIIIIIIII"))
        throw new IllegalStateException("Too many consecutive Is")
      else if (str.contains("XXXXXXXXXX"))
        throw new IllegalStateException("Too many consecutive Is")
      else str

    def _toInt(list: List[Int], acc: Int): Int =
      if (list.size == 1) acc + list.last
      else if (list(0) < list(1)) _toInt(list.tail, acc - list(0))
      else _toInt(list.tail, acc + list(0))

    if (romans == null || romans.isEmpty) 0
    else _toInt(_validate(romans).split("").map(mapDigit(_)).toList, 0)
  }


}
