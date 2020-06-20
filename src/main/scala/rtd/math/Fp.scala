package rtd.math

// https://en.wikipedia.org/wiki/Multiplicative_group_of_integers_modulo_n
abstract class Fp(val p: Int) extends Group[Int] {
  def compose(x: Int, y: Int): Int = {
    val result = (x * y) % p
    if (result < 0) {
      result + p
    } else {
      result
    }
  }

  // Can be improved to O((log p)^2) using the extended Euclidean algorithm.
  def invert(x: Int): Int = {
    // Follows from Fermat's little theorem.
    pow(x, p - 2)
  }

  def identity(): Int = 1

  // https://en.wikipedia.org/wiki/Euler%27s_criterion
  def isQuadraticResidue(x: Int): Boolean = {
    if (p == 2) {
      true
    } else {
      pow(x, (p - 1)/2) == identity()
    }
  }

  // Not yet implemented.
  def squareRoot(x: Int): Option[Int] = {
    None
  }
}