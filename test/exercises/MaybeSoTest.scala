package exercises

import org.scalatest.flatspec.AnyFlatSpec

class MaybeSoTest extends AnyFlatSpec {
  val maybeSo3 = MaybeSo(3)

  "MaybeSo" should "run as expected" in {
    assert(maybeSo3.map(_ * 2) === MaybeSo(6))
    assert(maybeSo3.flatMap(x => MaybeSo(x % 2 == 0)) === MaybeSo(false))
    assert(maybeSo3.filter(_ % 2 == 0) === MaybeNot)
  }
}
