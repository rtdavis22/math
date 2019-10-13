package rtd.math

abstract class VectorSpace[V, E] {
  val field: Field[E]

  def +(v: V, w: V): V

  // Addition is associative, so parens aren't necessary.
  def +(vs: V*): V = vs.fold(zero)(+)

  def *(c: E, v: V): V

  // v + (-v) = 0
  def -(v: V): V

  // v + 0 = v
  def zero: V

  // The vector v derived from a basis of the vector space and scalars M(v).
  // The one-column matrix M(v) is defined as the matrix of the vector v.
  def getFromBasisAndScalars(basis: List[V], m: List[E]): V = {
    m.zip(basis).map(p => *(p._1, p._2)).iterator.reduce(+)
  }

  def dual: DualSpace[V, E] = new DualSpace(this)

  class AdditionExpression(x: Expression[V], y: Expression[V]) extends BinaryExpression[V, V, V](x, y) {
    override def evaluate(): V = {
      VectorSpace.this.+(x.evaluate(), y.evaluate())
    }
  }

  class MultiplicationExpression(scalar: Expression[E], x: Expression[V]) extends BinaryExpression[E, V, V](scalar, x) {
    def evaluate(): V = {
      VectorSpace.this.*(scalar.evaluate(), x.evaluate())
    }
  }

  // v = 0 + v
  def addAdditiveIdentity(e: UnaryExpression[V]): AdditionExpression = {
    new AdditionExpression(new UnaryExpression[V](zero), e)
  }

  // 0 + v = v
  def removeAdditiveIdentity(e: AdditionExpression): Expression[V] = {
    if (e.x.evaluate() != field.zero) {
      throw new Exception("cannot remove additive identity")
    }
    e.y
  }

  // 1v = v
  def removeIdentity(e: MultiplicationExpression): Expression[V] = {
    if (e.x != field.one) {
      throw new Exception("cannot remove identity")
    }
    e.y
  }

  // v = 1v
  def addIdentity(e: UnaryExpression[V]): MultiplicationExpression = {
    new MultiplicationExpression(new UnaryExpression[E](field.one), e)
  }

  // u + v = v + u
  def commute(e: AdditionExpression): AdditionExpression = new AdditionExpression(e.y, e.x)

  // a(bv) = (ab)v
  def reassoc(scalar: Expression[E], e: MultiplicationExpression): MultiplicationExpression = {
    new MultiplicationExpression(new field.MultiplicationExpression(scalar, e.x), e.y)
  }

  // (ab)v = a(bv)
  def reassoc(e: field.MultiplicationExpression, t: V): MultiplicationExpression = {
    new MultiplicationExpression(e.x, new MultiplicationExpression(e.y, new UnaryExpression[V](t)))
  }

  // a(u + v) = au + av
  def distributeScalar(scalar: Expression[E], e: AdditionExpression): AdditionExpression = {
    new AdditionExpression(
      new MultiplicationExpression(scalar, e.x),
      new MultiplicationExpression(scalar, e.y)
    )
  }

  // au + av = a(u + v)
  def undistributeScalar(e1: MultiplicationExpression, e2: MultiplicationExpression): MultiplicationExpression = {
    if (e1.x != e2.x) {
      throw new Exception("cannot undistribute")
    }
    new MultiplicationExpression(e1.x, new AdditionExpression(e1.y, e2.y))
  }

  // (a + b)v = av + bv
  def distributeVector(e: field.AdditionExpression, t: V): AdditionExpression = {
    new AdditionExpression(
      new MultiplicationExpression(e.x, new UnaryExpression[V](t)),
      new MultiplicationExpression(e.y, new UnaryExpression[V](t))
    )
  }

  // av + bv = (a + b)v
  def undistributeVector(e1: MultiplicationExpression, e2: MultiplicationExpression): MultiplicationExpression = {
    if (e1.y != e2.y) {
      throw new Exception("cannot undistribute")
    }
    new MultiplicationExpression(new field.AdditionExpression(e1.x, e2.x), e1.y)
  }
}

object VectorSpace {
  // The product of vector spaces over the same field is a vector space.
  def *[V, W, E](V: VectorSpace[V, E], W: VectorSpace[W, E]): VectorSpace[(V, W), E] = {
    new VectorSpace[(V, W), E] {
      override val field: Field[E] = V.field

      override def +(v: (V, W), w: (V, W)): (V, W) = (V.+(v._1, w._1), W.+(v._2, w._2))

      override def *(e: E, v: (V, W)): (V, W) = (V.*(e, v._1), W.*(e, v._2))

      override def -(v: (V, W)): (V, W) = (V.-(v._1), W.-(v._2))

      override def zero: (V, W) = (V.zero, W.zero)
    }
  }

  // A vector space has a unique additive identity.
  // Suppose x is also an additive identity...
  def fn[V, E](vs: VectorSpace[V, E], x: V): Unit = {
    // x = x + 0
    val expression = vs.addAdditiveIdentity(new UnaryExpression[V](x))

    // x + 0 = 0 + x
    val commutedExpression = vs.commute(expression)

    // 0 + x = 0
    val expression2 = vs.removeAdditiveIdentity(commutedExpression)

    // x = 0
    assert(expression2.evaluate() == vs.zero)
  }
}