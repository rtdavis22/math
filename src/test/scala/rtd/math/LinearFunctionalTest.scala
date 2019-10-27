package rtd.math

import org.scalatest.{FunSuite, Matchers}

class LinearFunctionalTest extends FunSuite with Matchers {
  test("linear functional that maps a coordinate to the sum of its elements") {
    val f = new LinearFunctional(new RealVectorSpace[I3]) {
      override def apply(v: Coordinate[R, I3]): R = {
        W.+(v(0), v(1), v(2))
      }
    }
    f(new Coordinate[R, I3](R(3.0))).v shouldBe 9.0
  }
}
