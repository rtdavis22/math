package rtd.math

import org.scalatest.{FunSuite, Matchers}

class LinearFunctionalTest extends FunSuite with Matchers {
  test("linear functional that maps a coordinate to the sum of its elements") {
    val lf = new LinearFunctional(new RealVectorSpace[I3]) {
      override def apply(v: Coordinate[R, I3]): R = {
        W.+(W.+(v.array(0), v.array(1)), v.array(2))
      }
    }
    lf.apply(new Coordinate[R, I3](new R(3.0))).v shouldBe 9.0
  }
}