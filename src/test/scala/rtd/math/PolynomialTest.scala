package rtd.math

import org.scalatest.{FunSuite, Matchers}

class PolynomialTest extends FunSuite with Matchers {
  test("D' when applied to phi is the linear functional that takes p to p'(3)") {
    val dPrime = new DualMap(new RealDerivativeMap)
    val phi = new RealEvaluationMap(new R(3.0))
    val lf = dPrime.apply(phi)
    val result = lf.apply(List(new R(1.0), new R(2.0), new R(3.0)))
    result.v shouldEqual 20.0
  }
}
