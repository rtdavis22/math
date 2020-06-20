package rtd.math

abstract class LinearMap[V, W, E](val V: VectorSpace[V, E], val W: VectorSpace[W, E]) {
  def apply(v: V): W

  def dual: DualMap[V, W, E] = new DualMap(this)

  // T(v + w) = T(v) + T(w)
  def apply(e: V.AdditionExpression): W.AdditionExpression = {
    new W.AdditionExpression(
      new UnaryExpression[W](apply(e.x.evaluate())),
      new UnaryExpression[W](apply(e.y.evaluate())))
  }
}

// The set of linear maps from V to W forms a vector space. Also denoted L(V, W).
class Hom[V, W, E](val V: VectorSpace[V, E], val W: VectorSpace[W, E]) extends VectorSpace[LinearMap[V, W, E], E] {
  override val field: Field[E] = V.field

  override def +(s: LinearMap[V, W, E], t: LinearMap[V, W, E]): LinearMap[V, W, E] = {
    new LinearMap[V, W, E](V, W) {
      override def apply(e: V): W = W.+(s(e), t(e))
    }
  }

  override def *(c: E, s: LinearMap[V, W, E]): LinearMap[V, W, E] = {
    new LinearMap[V, W, E](V, W) {
      override def apply(e: V): W = W.*(c, s(e))
    }
  }

  override def -(s: LinearMap[V, W, E]): LinearMap[V, W, E] = {
    new LinearMap[V, W, E](V, W) {
      override def apply(e: V): W = W.-(s(e))
    }
  }

  override def zero: LinearMap[V, W, E] = {
    new LinearMap[V, W, E](V, W) {
      override def apply(e: V): W = W.zero
    }
  }

  // The product of two linear maps is defined as the composition of the maps: (ST)(u) = S(Tu)
  def *[Y](s: LinearMap[W, Y, E], t: LinearMap[V, W, E]): LinearMap[V, Y, E] = {
    new LinearMap[V, Y, E](V, s.W) {
      override def apply(e: V): Y = s(t(e))
    }
  }
}

// A linear operator is a linear map from V to itself.
abstract class LinearOperator[V, E](V: VectorSpace[V, E]) extends LinearMap[V, V, E](V, V)

// The endomorphisms of a vector space V (the set of all linear operators). Also denoted L(V).
class End[V, E](V: VectorSpace[V, E]) extends Hom[V, V, E](V, V)

class IdentityMap[V, E](V: VectorSpace[V, E]) extends LinearOperator[V, E](V) {
  override def apply(v: V): V = v
}
