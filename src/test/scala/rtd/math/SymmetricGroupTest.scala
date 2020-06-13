package rtd.math

import org.scalatest.{FunSuite, Matchers}

class SymmetricGroupTest extends FunSuite with Matchers {
  test("composition test 1") {
    val sigma = Permutation(Map(2 -> 3, 3 -> 2))
    val tau = Permutation(Map(3 -> 4, 4 -> 3))
    sigma.compose(tau).m shouldEqual Map(2 -> 4, 3 -> 2, 4 -> 3)
    tau.compose(sigma).m shouldEqual Map(2 -> 3, 3 -> 4, 4 -> 2)
  }

  test("composition test 2") {
    val sigma = Permutation(Map(1 -> 2))
    val tau = Permutation(Map(2 -> 1,  3 -> 4))
    sigma.compose(tau).m shouldEqual Map(3 -> 4)
    tau.compose(sigma).m shouldEqual Map(3 -> 4)
  }
}
