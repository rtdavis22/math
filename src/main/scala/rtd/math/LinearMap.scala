package rtd.math

abstract class LinearMap[V, W, E](val v: VectorSpace[V, E], val w: VectorSpace[W, E]) {
  def apply(e: V): W

  // T(v + w) = T(v) + T(w)
  def apply(e: v.AdditionExpression): w.AdditionExpression = {
    new w.AdditionExpression(
      new UnaryExpression[W](apply(e.x.evaluate())),
      new UnaryExpression[W](apply(e.y.evaluate())))
  }
}

// class AllLinearMaps extends VectorSpace... pg. 55

// Probably merge this into the above
object LinearMap {
  // (S + T)(v) = Sv + Tv
  def +[V,W,E](s: LinearMap[V,W,E], t: LinearMap[V,W,E]): LinearMap[V,W,E] = {
    new LinearMap[V,W,E](s.v, s.w) {
      def apply(e: V): W = {
        w.+(s.apply(e), t.apply(e))
      }
    }
  }

  // (cT)(v) = c(Tv)
  def *[T,V,E](c: E, s: LinearMap[T,V,E]): LinearMap[T,V,E] = {
    new LinearMap[T,V,E](s.v, s.w) {
      def apply(e: T): V = {
        w.*(c, s.apply(e))
      }
    }
  }
}

class ExampleLinearMap extends LinearMap(new RealVectorSpace[I3], new RealVectorSpace[I4]) {
  override def apply(e: Coordinate[R, I3]): Coordinate[R, I4] = {
    w.zero()
  }
}

abstract class EndomorphicMap[T, E](v: VectorSpace[T, E]) extends LinearMap[T, T, E](v, v) {}

class IdentityMap[T, E](v: VectorSpace[T, E]) extends EndomorphicMap[T,E](v) {
  override def apply(e: T): T = e
}

class ZeroMap[T, V, E](v1: VectorSpace[T, E], v2: VectorSpace[V, E]) extends LinearMap[T, V, E](v1, v2) {
  override def apply(v: T): V = v2.zero()
}

object LinearMapMain {
  def main(args: Array[String]): Unit = {
    val m = new ExampleLinearMap
    //val res = m.apply(new Coordinate[R, I3]())
    //println(res)

    val vs = new RealVectorSpace[I3]()
    val m2 = new IdentityMap(new RealVectorSpace[I3]())
    val m3 = LinearMap.+(m2, m2)
    val res2 = m3.apply(vs.zero())
    println(res2)
  }
}