package rtd.math

abstract class LinearMap[V, W, E](val V: VectorSpace[V, E], val W: VectorSpace[W, E]) {
  def apply(v: V): W

  // The matrix of a linear map with respect to a basis
  def getMatrix(basis: List[V]): List[W] = {
    basis.map(apply)
  }

  // T(v + w) = T(v) + T(w)
  def apply(e: V.AdditionExpression): W.AdditionExpression = {
    new W.AdditionExpression(
      new UnaryExpression[W](apply(e.x.evaluate())),
      new UnaryExpression[W](apply(e.y.evaluate())))
  }
}

// The set of linear maps from V to W forms a vector space
class Hom[V, W, E](val V: VectorSpace[V, E], val W: VectorSpace[W, E]) extends VectorSpace[LinearMap[V, W, E], E] {
  override val field: Field[E] = V.field

  override def +(s: LinearMap[V, W, E], t: LinearMap[V, W, E]): LinearMap[V, W, E] = {
    new LinearMap[V, W, E](V, W) {
      override def apply(e: V): W = W.+(s.apply(e), t.apply(e))
    }
  }

  override def *(c: E, s: LinearMap[V, W, E]): LinearMap[V, W, E] = {
    new LinearMap[V, W, E](V, W) {
      override def apply(e: V): W = W.*(c, s.apply(e))
    }
  }

  override def -(s: LinearMap[V, W, E]): LinearMap[V, W, E] = {
    new LinearMap[V, W, E](V, W) {
      override def apply(e: V): W = W.-(s.apply(e))
    }
  }

  override def zero(): LinearMap[V, W, E] = {
    new LinearMap[V, W, E](V, W) {
      override def apply(e: V): W = W.zero()
    }
  }

  // The product of two linear maps is defined as the composition of the maps: (ST)(u) = S(Tu)
  def *[Y](s: LinearMap[W, Y, E], t: LinearMap[V, W, E]): LinearMap[V, Y, E] = {
    new LinearMap[V, Y, E](V, s.W) {
      override def apply(e: V): Y = s.apply(t.apply(e))
    }
  }
}

// The endomorphisms of a vector space V (linear maps from V to itself)
class End[V, E](V: VectorSpace[V, E]) extends Hom[V, V, E](V, V)

abstract class EndomorphicMap[V, E](V: VectorSpace[V, E]) extends LinearMap[V, V, E](V, V)

class IdentityMap[V, E](V: VectorSpace[V, E]) extends EndomorphicMap[V, E](V) {
  override def apply(v: V): V = v
}

object LinearMapMain {
  def main(args: Array[String]): Unit = {
    val vs = new RealVectorSpace[I3]
    val m2 = new IdentityMap(vs)

    val hom = new Hom(vs, vs)
    val zeroMap = hom.zero()
    val res = zeroMap.apply(vs.zero())
    println(res)
  }
}