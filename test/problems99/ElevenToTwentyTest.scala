package problems99

import org.scalatest.flatspec.AnyFlatSpec

class ElevenToTwentyTest extends AnyFlatSpec {

  "pack" should "support run length encoding for duplicates" in {
    assert(ElevenToTwenty.pack(List("A", "A")) == List(Right(("A", 2))))
    assert(ElevenToTwenty.pack(List(1, 1, 1, 2, 2, 2)) == List(Right((1, 3)), Right((2, 3))))
  }

  "pack" should "not encode dicrete values" in {
    assert(ElevenToTwenty.pack(List("A", "B")) == List(Left("A"), Left("B")))
    assert(ElevenToTwenty.pack(List(1, 1, 1, 4, 2, 2, 2)) == List(Right((1, 3)), Left(4), Right((2, 3))))
  }

  "decode" should "throw exception for null input" in {
    assertThrows[IllegalStateException](ElevenToTwenty.decode(null))
  }

  "decode" should "throw exception for empty input" in {
    assertThrows[IllegalStateException](ElevenToTwenty.decode(List()) == List())
  }

  "decode" should "decode a single element from a tuple" in {
    assert(ElevenToTwenty.decode(List((1, 1))) == List(1))
    assert(ElevenToTwenty.decode(List((1, 1), (2, 1))) == List(1, 2))
    assert(ElevenToTwenty.decode(List(("A", 1), ("B", 1), ("C", 1))) == List("A", "B", "C"))
  }

  "decode" should "decode consecutive identical elements from the same tuple" in {
    assert(ElevenToTwenty.decode(List(("A", 2))) == List("A", "A"))
    assert(ElevenToTwenty.decode(List((1, 3), (2, 3))) == List(1, 1, 1, 2, 2, 2))
  }

  "decode" should "decode non consecutive identical elements from a different tuple" in {
    assert(ElevenToTwenty.decode(List(("A", 1), ("B", 1), ("A", 1))) == List("A", "B", "A"))
    assert(ElevenToTwenty.decode(List(('t', 1), ('a', 3), ('t', 1), ('a', 3), ('b', 2), ('t', 1))) == List('t', 'a', 'a', 'a', 't', 'a', 'a', 'a', 'b', 'b', 't'))
  }

  "encodeViaSpan" should "throw exception for null input" in {
    assertThrows[IllegalStateException](ElevenToTwenty.encodeViaSpan(null))
  }

  "encodeViaSpan" should "throw exception for empty input" in {
    assertThrows[IllegalStateException](ElevenToTwenty.encodeViaSpan(List()) == List())
  }

  "encodeViaSpan" should "encode a single element to a tuple" in {
    assert(ElevenToTwenty.encodeViaSpan(List(1)) == List((1, 1)))
    assert(ElevenToTwenty.encodeViaSpan(List(1, 2)) == List((1, 1), (2, 1)))
    assert(ElevenToTwenty.encodeViaSpan(List("A", "B", "C")) == List(("A", 1), ("B", 1), ("C", 1)))
  }

  "encodeViaSpan" should "encode consecutive identical elements to the same tuple" in {
    assert(ElevenToTwenty.encodeViaSpan(List("A", "A")) == List(("A", 2)))
    assert(ElevenToTwenty.encodeViaSpan(List(1, 1, 1, 2, 2, 2)) == List((1, 3), (2, 3)))
  }

  "encodeViaSpan" should "encode non consecutive identical elements to a different tuple" in {
    assert(ElevenToTwenty.encodeViaSpan(List("A", "B", "A")) == List(("A", 1), ("B", 1), ("A", 1)))
    assert(ElevenToTwenty.encodeViaSpan(List('t', 'a', 't', 'a', 'b', 't')) == List(('t', 1), ('a', 1), ('t', 1), ('a', 1), ('b', 1), ('t', 1)))
  }

  "duplicate" should "throw exception for null input" in {
    assertThrows[IllegalStateException](ElevenToTwenty.duplicate(null))
  }

  "duplicate" should "throw exception for empty input" in {
    assertThrows[IllegalStateException](ElevenToTwenty.duplicate(List()) == List())
  }

  "duplicate" should "duplicate every element in a single element list" in {
    assert(ElevenToTwenty.duplicate(List(1)) == List(1, 1))
    assert(ElevenToTwenty.duplicate(List("A")) == List("A", "A"))
    assert(ElevenToTwenty.duplicate(List(List(1))) == List(List(1), List(1)))
  }

  "duplicate" should "duplicate every element in a multi element list" in {
    assert(ElevenToTwenty.duplicate(List(1, 2, 3)) == List(1, 1, 2, 2, 3, 3))
    assert(ElevenToTwenty.duplicate(List("A", "B", "C")) == List("A", "A", "B", "B", "C", "C"))
    assert(ElevenToTwenty.duplicate(List(List(1), List(5))) == List(List(1), List(1), List(5), List(5)))
  }

}
