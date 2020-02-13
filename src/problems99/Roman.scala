package problems99

object Roman {
  def toInt(romans: String) = {
    def _validate(str: String): String =
      if (str.split("").count(_ == "V") > 1)
        throw new IllegalStateException("Too many Vs")
      else str

    if (romans == null) 0
    else _validate(romans).split("").count(_ == "I") +
      romans.split("").count(_ == "V") * 5
  }



}
