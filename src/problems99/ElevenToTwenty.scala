package problems99

object ElevenToTwenty {
  def duplicate[A](n: List[A]): List[A] =
    if (n == null) throw new IllegalStateException("Cannot encode null list")
    else if (n.isEmpty) throw new IllegalStateException("Cannot encode empty list")
    else n.flatMap(x => List(x, x))

  def duplicateN[A](x: Int, n: List[A]): List[A] =
    if (n == null) throw new IllegalStateException("Cannot encode null list")
    else if (n.isEmpty) throw new IllegalStateException("Cannot encode empty list")
    else n.flatMap(List.fill(x)(_))

  def encodeViaSpan[A](n: List[A]): List[(A, Int)] = {

    def _encode[A](_n: List[A], accumulator: List[(A, Int)]): List[(A, Int)] = {
      if (_n.isEmpty) accumulator
      else {
        val (h, t) = _n.span(_ == _n.head)
        _encode(t, accumulator :+ (h.head, h.size))
      }
    }

    if (n == null) throw new IllegalStateException("Cannot encode null list")
    else if (n.isEmpty) throw new IllegalStateException("Cannot encode empty list")
    else _encode(n, List())
  }

  def decode[A](n: List[(A, Int)]): List[A] =
    if (n == null) throw new IllegalStateException("Cannot decode null list")
    else if (n.isEmpty) throw new IllegalStateException("Cannot decode empty list")
    else n.flatMap(x => List.fill(x._2)(x._1))

  def pack[A](n: List[A]): List[Either[A, (A, Int)]] =
    if (n == null) throw new IllegalStateException("Cannot encode null list")
    else if (n.isEmpty) throw new IllegalStateException("Cannot encode empty list")
    else new OneToTen().compressToList(n).map(x =>
      if (x.length == 1) Left(x.head)
      else Right((x.head, x.size))
    )
}