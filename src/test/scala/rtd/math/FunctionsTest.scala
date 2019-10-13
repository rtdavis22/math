package rtd.math

import org.scalatest.{FunSuite, Matchers}

class FunctionsTest extends FunSuite with Matchers {
  test("function addition") {
    val V = new Functions[(Int, Int), R](new  Reals)

    val func = (q: (Int, Int)) => R(q._1 + q._2)
    val func2 = V.+(func, func)

    func2((5, 3)).v shouldEqual 16.0
  }
}
