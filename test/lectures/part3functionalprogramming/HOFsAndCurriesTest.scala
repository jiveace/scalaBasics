package lectures.part3functionalprogramming

import org.scalatest.flatspec.AnyFlatSpec

class HOFsAndCurriesTest extends AnyFlatSpec {

  /*
  Expand MyList
    - forEach method.  A => Unit
      [1,2,3].forEach(x => println(x))

    - sort function. ((A, A) => Int) => MyList
      [1,2,3].sort((x,y) => y -x) => [3,2,1]

    - zipWith. (list, (a, A) => B) => MyList[B])
    [1,2,3].zipWith([4,5,6], x * y) => [1 *4, 2*5, 3 * 6] = [4, 10, 18]

    - fold(start)(function) => a value
    [1,2,3].fold(0)(x+y) = 6
    // Effectively reduce


    2. toCurry(f: (Int, Int) => Int) => (Int => Int => Int)
       fromCurry(f: (Int => Int => Int)) => (Int, Int) => Int

    3. compose(f,g) => f(g(x))
       andThen(f,g) => x => g(f(x))

   */

  def toCurry(f: (Int, Int) => Int): (Int => Int => Int) =
    x => y => f(x, y)

  def fromCurry(f: (Int => Int => Int)): (Int, Int) => Int =
    (x, y) =>f(x)(y)

  "toCurry" must "make curry" in {
    val notCurry: (Int, Int) => Int = _ * _
    val curry = toCurry(notCurry)

    assert(curry(6)(8) === 6 * 8)
  }

  "fromCurry" must "unmake curry" in {
    val notCurry: (Int, Int) => Int = _ * _
    val curry = toCurry(notCurry)
    val reconstitutedCurry = fromCurry(curry)

    assert(reconstitutedCurry(6,8) === 6 * 8)
  }
}