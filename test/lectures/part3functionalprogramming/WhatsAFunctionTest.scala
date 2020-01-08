package lectures.part3functionalprogramming

import org.scalatest.flatspec.AnyFlatSpec

class WhatsAFunctionTest extends AnyFlatSpec {

  val concatenate: Function2[String, String, String] = new Function2[String, String, String] {
    override def apply(in1: String, in2: String): String = in1 + in2
  }

  val sexyConcatenate: (String, String) => String = new ((String, String) => String) {
    override def apply(in1: String, in2: String): String = in1 + in2
  }

  // Underscores are different wildcards.
  // First one says 'whatever value is passed in, ignore it and return a function.
  // The second underscore id the parameter of the returned function
  val produceDoubler: Int => (Int => Int) = (_: Int) => 2 * _
  val sexyProduceDoubler: Int => Int => Int = (_: Int) => (v1: Int) => 2 * v1

  "concatenate" must "concatenate two strings" in {
    assert(concatenate("blue", "red") === "bluered")
  }

  "sexyConcatenate" must "concatenate two strings" in {
    assert(concatenate("blue", "red") === "bluered")
  }

  "produceFunction" must "produce a function that doubles in input int" in {
    assert(produceDoubler(6)(5) === 10)
  }

  "sexyProduceFunction" must "produce a function that doubles in input int" in {
    assert(sexyProduceDoubler(6)(5) === 10)
  }
}
