package rtd.math

// The vector space of polynomials over a field. A polynomial is represented as a list of coefficients.
class Polynomial[E](override val field: Field[E]) extends VectorSpace[List[E], E] {
  override def +(p: List[E], q: List[E]): List[E] = {
    p.zipAll(q, field.zero, field.zero).map(t => field.+(t._1, t._2)).
      reverse.dropWhile(c => c == field.zero).reverse
  }

  override def *(c: E, p: List[E]): List[E] = p.map(field.*(c, _))

  override def -(p: List[E]): List[E] = p.map(field.-)

  override def zero: List[E] = List()
}

// A linear functional that maps a polynomial p to p(x), the polynomial evaluated at x.
// This could be made more general to work over any polynomial.
class EvaluationMap[T](field: Field[T], x: T) extends LinearFunctional(new Polynomial(field)) {
  override def apply(p: List[T]): T = {
    p.zipWithIndex.map(c => V.field.*(c._1, V.field.pow(x, c._2))).reduce(V.field.+)
  }
}

class RealEvaluationMap(x: R) extends EvaluationMap(new ℝ, x)

// A linear map that maps a polynomial p to its derivative, p'.
class RealDerivativeMap extends EndomorphicMap(new Polynomial(new ℝ)) {
  override def apply(p: List[R]): List[R] = {
    p.zipWithIndex.map(c => V.field.*(new R(c._2), c._1)).drop(1)
  }
}

object PolynomialMain {
  def main(args: Array[String]): Unit = {
  }
}