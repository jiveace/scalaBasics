package exercises

import scala.annotation.tailrec

class PowerOfTwo {
  def powerOfTwo(n: Int): Boolean = {

    @tailrec
    def helper(x: Int): Boolean =
      if (x < 0 || x > n) false
      else if (x == n) true
      else helper(x * 2)

    if (n == 0) false
    else helper(1)
  }
}
