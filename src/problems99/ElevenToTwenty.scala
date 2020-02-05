package problems99

object ElevenToTwenty {
  def removeAt[A](list: List[A], x: Int): (List[A], A) =
    (list.take(x - 1) ::: list.takeRight(list.length - x), list(x - 1))


  def roll[A](rollBy: Int, n: List[A]): List[A] = {
    def _roll[A](count: Int, currentList: List[A]): List[A] =
      if (count > 0) _roll(count - 1, currentList.tail :+ currentList.head)
      else if (count < 0) _roll(count + 1, currentList.last +: currentList.init)
      else currentList

    if (n == null) throw new IllegalStateException("Cannot split null list")
    else if (n.isEmpty) throw new IllegalStateException("Cannot split empty list")
    else _roll(rollBy, n)
  }

  def slice[A](start: Int, end: Int, list: List[A]): List[A] = {

    def _slice[A](cutFromLeft: Int, cutFromRight: Int, _remaining: List[A]): List[A] = (cutFromLeft, cutFromRight, _remaining) match {
      case (0, 0, _remaining) => _remaining
      case (0, cutFromRight, _remaining) => _slice(cutFromLeft, cutFromRight - 1, _remaining.init)
      case (cutFromLeft, cutFromRight, _remaining) => {
        _slice(cutFromLeft - 1, cutFromRight, _remaining.tail)
      }
    }

    if (list == null) throw new IllegalStateException("Cannot split null list")
    else if (list.isEmpty) throw new IllegalStateException("Cannot split empty list")
    else if (start < 0 || start >= list.length) throw new IndexOutOfBoundsException("Start Index is outwith list ranges")
    else if (end < 0 || end > list.length) throw new IndexOutOfBoundsException("End Index is outwith list ranges")
    else if (end < start) throw new IndexOutOfBoundsException("End Index must not be smaller than start index")
    //    else list.take(end).takeRight(end - start)
    _slice(start, list.length - end, list)
  }

  def splitInTwain[A](x: Int, n: List[A]): (List[A], List[A]) =
    if (n == null) throw new IllegalStateException("Cannot split null list")
    else if (n.isEmpty) throw new IllegalStateException("Cannot split empty list")
    else (n.take(x), n.drop(x))

  def drop[A](x: Int, n: List[A]): Any =
    if (n == null) throw new IllegalStateException("Cannot drop null list")
    else if (n.isEmpty) throw new IllegalStateException("Cannot drop empty list")
    else if (x <= 0) n
    else n.zipWithIndex.filter(e => (e._2 + 1) % x != 0).map(_._1)

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