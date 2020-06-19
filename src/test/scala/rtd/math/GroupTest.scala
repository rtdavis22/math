package rtd.math

import org.scalatest.{FunSuite, Matchers}

// The multiplicative group of integers mod 5.
class F5 extends Group[Int] {
  override def compose(x: Int, y: Int): Int = {
    val result = (x * y) % 5
    if (result < 0) {
      result + 5
    } else {
      result
    }
  }

  override def identity(): Int = 1

  override def invert(t: Int): Int = {
    t match {
      case 1 => 1
      case 2 => 3
      case 3 => 2
      case 4 => 4
    }
  }
}

class GroupTest extends FunSuite with Matchers {
  test("exponentiation tests") {
    val f4 = new F5()
    f4.pow(2, -99) shouldBe 2
    f4.pow(2, -2) shouldBe 4
    f4.pow(2, -1) shouldBe 3
    f4.pow(2, 0) shouldBe 1
    f4.pow(2, 1) shouldBe 2
    f4.pow(2, 2) shouldBe 4
    f4.pow(2, 100) shouldBe 1
  }
}
