package rtd.math

// A linear functional is a linear map from a vector space to its field of scalars.
abstract class LinearFunctional[V, E](v: VectorSpace[V, E]) extends LinearMap[V, E, E](v, v.field)

// The dual space of V, the vector space consisting of all linear functionals on V, is a vector space.
class DualSpace[V, E](V: VectorSpace[V, E]) extends VectorSpace[LinearFunctional[V, E], E] {
  val field: Field[E] = V.field

  override def +(f: LinearFunctional[V, E], g: LinearFunctional[V, E]): LinearFunctional[V, E] = {
    new LinearFunctional(V) {
      override def apply(v: V): E = field.+(f(v), g(v))
    }
  }

  override def *(e: E, f: LinearFunctional[V, E]): LinearFunctional[V, E] = {
    new LinearFunctional(V) {
      override def apply(v: V): E = field.*(e, f(v))
    }
  }

  override def -(f: LinearFunctional[V, E]): LinearFunctional[V, E] = {
    new LinearFunctional(V) {
      override def apply(v: V): E = field.-(f(v))
    }
  }

  override def zero: LinearFunctional[V, E] = {
    new LinearFunctional(V) {
      override def apply(v: V): E = field.zero
    }
  }

  // TODO: Is this correct?
  def basis(basisForV: List[V]): List[LinearFunctional[V, E]] = {
    basisForV.map(v => new LinearFunctional(V) {
      override def apply(w: V): E = if (w == v) field.one else field.zero
    })
  }
}

class DualMap[V, W, E](t: LinearMap[V, W, E]) extends LinearMap(new DualSpace(t.W), new DualSpace(t.V)) {
  override def apply(g: LinearFunctional[W, E]): LinearFunctional[V, E] = {
    new LinearFunctional[V, E](t.V) {
      override def apply(v: V): E = g(t(v))
    }
  }
}
