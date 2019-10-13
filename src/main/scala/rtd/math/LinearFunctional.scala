package rtd.math

// A linear functional is a linear map from a vector space to its field of scalars.
abstract class LinearFunctional[V, E](v: VectorSpace[V, E]) extends LinearMap[V, E, E](v, v.field)

// The dual space of V, the vector space consisting of all linear functionals on V, is a vector space.
class DualSpace[V, E](V: VectorSpace[V, E]) extends VectorSpace[LinearFunctional[V, E], E] {
  val field: Field[E] = V.field

  override def +(lf1: LinearFunctional[V, E], lf2: LinearFunctional[V, E]): LinearFunctional[V, E] = {
    new LinearFunctional(V) {
      override def apply(v: V): E = field.+(lf1.apply(v), lf2.apply(v))
    }
  }

  override def *(e: E, lf: LinearFunctional[V, E]): LinearFunctional[V, E] = {
    new LinearFunctional(V) {
      override def apply(v: V): E = field.*(e, lf.apply(v))
    }
  }

  override def -(lf: LinearFunctional[V, E]): LinearFunctional[V, E] = {
    new LinearFunctional(V) {
      override def apply(v: V): E = field.-(lf.apply(v))
    }
  }

  override def zero(): LinearFunctional[V, E] = {
    new LinearFunctional(V) {
      override def apply(v: V): E = field.zero()
    }
  }

  // TODO: Is this correct?
  def basis(basisForV: List[V]): List[LinearFunctional[V, E]] = {
    basisForV.map(v => new LinearFunctional(V) {
      override def apply(w: V): E = if (w == v) field.one() else field.zero()
    })
  }
}

class DualMap[V, W, E](t: LinearMap[V, W, E]) extends LinearMap(new DualSpace(t.W), new DualSpace(t.V)) {
  override def apply(lf: LinearFunctional[W, E]): LinearFunctional[V, E] = {
    new LinearFunctional[V, E](t.V) {
      override def apply(v: V): E = lf.apply(t.apply(v))
    }
  }
}

object LinearFunctionMain {
  def main(args: Array[String]): Unit = {
    val lf = new LinearFunctional(new RealVectorSpace[I3]) {
      override def apply(v: Coordinate[R, I3]): R = {
        W.+(W.+(v.array(0), v.array(1)), v.array(2))
      }
    }
    val c = new Coordinate[R, I3](new R(3.0))
    println(lf.apply(c))
  }
}
