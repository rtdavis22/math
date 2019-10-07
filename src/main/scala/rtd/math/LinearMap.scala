package rtd.math

abstract class LinearMap[V, W, E](val v: VectorSpace[V, E], val w: VectorSpace[W, E]) {
  def apply(e: V): W

  // The product of two linear maps is defined as the composition of the maps: (ST)(u) = S(Tu)
  def *[Y](t: LinearMap[W, Y, E]): LinearMap[V, Y, E] = {
    new LinearMap[V, Y, E](v, t.w) {
      override def apply(e: V): Y = {
        t.apply(LinearMap.this.apply(e))
      }
    }
  }

  // T(v + w) = T(v) + T(w)
  def apply(e: v.AdditionExpression): w.AdditionExpression = {
    new w.AdditionExpression(
      new UnaryExpression[W](apply(e.x.evaluate())),
      new UnaryExpression[W](apply(e.y.evaluate())))
  }
}

// The set of linear maps from V to W forms a vector space
class Hom[V, W, E](val v: VectorSpace[V, E], val w: VectorSpace[W, E], field: Field[E]) extends VectorSpace[LinearMap[V, W, E], E](field) {
  override def +(s: LinearMap[V, W, E], t: LinearMap[V, W, E]): LinearMap[V, W, E] = {
    new LinearMap[V, W, E](v, w) {
      override def apply(e: V): W = w.+(s.apply(e), t.apply(e))
    }
  }

  override def *(c: E, s: LinearMap[V, W, E]): LinearMap[V, W, E] = {
    new LinearMap[V, W, E](v, w) {
      override def apply(e: V): W = w.*(c, s.apply(e))
    }
  }

  override def -(s: LinearMap[V, W, E]): LinearMap[V, W, E] = {
    new LinearMap[V, W, E](v, w) {
      override def apply(e: V): W = w.-(s.apply(e))
    }
  }

  override def zero(): LinearMap[V, W, E] = {
    new LinearMap[V, W, E](v, w) {
      override def apply(e: V): W = w.zero()
    }
  }
}

// The endomorphisms of a linear map V
class End[V, E](v: VectorSpace[V, E], field: Field[E]) extends Hom[V, V, E](v, v, field)

abstract class EndomorphicMap[T, E](v: VectorSpace[T, E]) extends LinearMap[T, T, E](v, v)

class IdentityMap[T, E](v: VectorSpace[T, E]) extends EndomorphicMap[T,E](v) {
  override def apply(e: T): T = e
}

class ZeroMap[T, V, E](v1: VectorSpace[T, E], v2: VectorSpace[V, E]) extends LinearMap[T, V, E](v1, v2) {
  override def apply(v: T): V = v2.zero()
}

object LinearMapMain {
  def main(args: Array[String]): Unit = {
    val vs = new RealVectorSpace[I3]()
    val m2 = new IdentityMap(new RealVectorSpace[I3]())

    val hom = new Hom(vs, vs, new ℝ)
    val zeroMap = hom.zero()
    val res = zeroMap.apply(vs.zero())
    println(res)
  }
}