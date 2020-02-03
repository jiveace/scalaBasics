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

  "duplicateN" should "throw exception for null list input" in {
    assertThrows[IllegalStateException](ElevenToTwenty.duplicateN(1, null))
  }

  "duplicateN" should "throw exception for empty list input" in {
    assertThrows[IllegalStateException](ElevenToTwenty.duplicateN(1, List()))
  }

  "duplicateN" should "return empty list for a duplicator of 0" in {
    assert(ElevenToTwenty.duplicateN(0, List(1)) == List())
    assert(ElevenToTwenty.duplicateN(0, List("A")) == List())
    assert(ElevenToTwenty.duplicateN(0, List(11.12)) == List())
  }

  "duplicateN" should "return input list for a duplicator of 2" in {
    assert(ElevenToTwenty.duplicateN(1, List(1)) == List(1))
    assert(ElevenToTwenty.duplicateN(1, List("A")) == List("A"))
    assert(ElevenToTwenty.duplicateN(1, List(11.12)) == List(11.12))
  }

  "duplicateN" should "return 'duplicate' list for a duplicator of 2" in {
    assert(ElevenToTwenty.duplicateN(2, List(1)) == ElevenToTwenty.duplicate(List(1)))
    assert(ElevenToTwenty.duplicateN(2, List("A")) == ElevenToTwenty.duplicate(List("A")))
    assert(ElevenToTwenty.duplicateN(2, List(11.12)) == ElevenToTwenty.duplicate(List(11.12)))
  }

  "drop" should "throw exception for null list input" in {
    assertThrows[IllegalStateException](ElevenToTwenty.drop(1, null))
  }

  "drop" should "throw exception for empty list input" in {
    assertThrows[IllegalStateException](ElevenToTwenty.drop(1, List()))
  }

  "drop" should "return input list for a drop count of '0' of 0" in {
    assert(ElevenToTwenty.drop(0, List(1)) == List(1))
    assert(ElevenToTwenty.drop(0, List("A")) == List("A"))
    assert(ElevenToTwenty.drop(0, List(11.12)) == List(11.12))
  }

  "drop" should "return empty list for a drop count of '1'" in {
    assert(ElevenToTwenty.drop(1, List(1)) == List())
    assert(ElevenToTwenty.drop(1, List("A")) == List())
    assert(ElevenToTwenty.drop(1, List(11.12)) == List())
  }

  "drop" should "drop every alternate element  for a drop count of '2'" in {
    assert(ElevenToTwenty.drop(2, List(1, 2, 3, 4)) == List(1, 3))
    assert(ElevenToTwenty.drop(2, List("A", "B", "C", "D")) == List("A", "C"))
    assert(ElevenToTwenty.drop(2, List(11.12, 22.23, 33.34, 44.45)) == List(11.12, 33.34))
  }

  "drop" should "drop every third element  for a drop count of '3'" in {
    assert(ElevenToTwenty.drop(3, List(1, 2, 3, 4, 5, 6)) == List(1, 2, 4, 5))
    assert(ElevenToTwenty.drop(3, List("A", "B", "C", "D", "E", "F")) == List("A", "B", "D", "E"))
    assert(ElevenToTwenty.drop(3, List(11.12, 22.23, 33.34, 44.45, 55.56, 66.67)) == List(11.12, 22.23, 44.45, 55.56))
  }

}