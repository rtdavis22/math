package rtd.math

abstract class LinearMap[V, W, E, F <: Field[E]](val v: VectorSpace[V, E, F], val w: VectorSpace[W, E, F]) {
  def apply(e: V): W

  // T(v + w) = T(v) + T(w)
  def apply(e: v.AdditionExpression): w.AdditionExpression = {
    new w.AdditionExpression(
      new UnaryExpression[W](apply(e.x.evaluate())),
      new UnaryExpression[W](apply(e.y.evaluate())))
  }
}

// The set of linear maps from V to W forms a vector space
class Hom[V, W, E, F <: Field[E]](val v: VectorSpace[V, E, F], val w: VectorSpace[W, E, F])
  extends VectorSpace[LinearMap[V, W, E, F], E, F](v.field) {

  override def +(s: LinearMap[V, W, E, F], t: LinearMap[V, W, E, F]): LinearMap[V, W, E, F] = {
    new LinearMap[V, W, E, F](v, w) {
      override def apply(e: V): W = w.+(s.apply(e), t.apply(e))
    }
  }

  override def *(c: E, s: LinearMap[V, W, E, F]): LinearMap[V, W, E, F] = {
    new LinearMap[V, W, E, F](v, w) {
      override def apply(e: V): W = w.*(c, s.apply(e))
    }
  }

  override def -(s: LinearMap[V, W, E, F]): LinearMap[V, W, E, F] = {
    new LinearMap[V, W, E, F](v, w) {
      override def apply(e: V): W = w.-(s.apply(e))
    }
  }

  override def zero(): LinearMap[V, W, E, F] = {
    new LinearMap[V, W, E, F](v, w) {
      override def apply(e: V): W = w.zero()
    }
  }

  // The product of two linear maps is defined as the composition of the maps: (ST)(u) = S(Tu)
  def *[Y](s: LinearMap[W, Y, E, F], t: LinearMap[V, W, E, F]): LinearMap[V, Y, E, F] = {
    new LinearMap[V, Y, E, F](v, s.w) {
      override def apply(e: V): Y = s.apply(t.apply(e))
    }
  }
}

// The endomorphisms of a vector space V (linear maps from V to itself)
class End[V, E, F <: Field[E]](v: VectorSpace[V, E, F]) extends Hom[V, V, E, F](v, v)

abstract class EndomorphicMap[T, E, F <: Field[E]](v: VectorSpace[T, E, F]) extends LinearMap[T, T, E, F](v, v)

class IdentityMap[T, E, F <: Field[E]](v: VectorSpace[T, E, F]) extends EndomorphicMap[T,E,F](v) {
  override def apply(e: T): T = e
}

object LinearMapMain {
  def main(args: Array[String]): Unit = {
    val vs = new RealVectorSpace[I3]()
    val m2 = new IdentityMap(new RealVectorSpace[I3]())

    val hom = new Hom(vs, vs)
    val zeroMap = hom.zero()
    val res = zeroMap.apply(vs.zero())
    println(res)
  }
}