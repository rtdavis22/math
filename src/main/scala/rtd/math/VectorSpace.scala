package rtd.math

abstract class VectorSpace[T, U, F <: Field[U]](val field: F) {
  def +(v: T, w: T): T

  def *(c: U, v: T): T

  // v + (-v) = 0
  def -(v: T): T

  // v + 0 = v
  def zero(): T

  class AdditionExpression(x: Expression[T], y: Expression[T]) extends BinaryExpression[T, T, T](x, y) {
    override def evaluate(): T = {
      VectorSpace.this.+(x.evaluate(), y.evaluate())
    }
  }

  class MultiplicationExpression(scalar: Expression[U], x: Expression[T]) extends BinaryExpression[U, T, T](scalar, x) {
    def evaluate(): T = {
      VectorSpace.this.*(scalar.evaluate(), x.evaluate())
    }
  }

  // v = 0 + v
  def addAdditiveIdentity(e: UnaryExpression[T]): AdditionExpression = {
    new AdditionExpression(new UnaryExpression[T](zero()), e)
  }

  // 0 + v = v
  def removeAdditiveIdentity(e: AdditionExpression): Expression[T] = {
    if (e.x.evaluate() != field.zero()) {
      throw new Exception("cannot remove additive identity")
    }
    e.y
  }

  // 1v = v
  def removeIdentity(e: MultiplicationExpression): Expression[T] = {
    if (e.x != field.one()) {
      throw new Exception("cannot remove identity")
    }
    e.y
  }

  // v = 1v
  def addIdentity(e: UnaryExpression[T]): MultiplicationExpression = {
    new MultiplicationExpression(new UnaryExpression[U](field.one()), e)
  }

  // u + v = v + u
  def commute(e: AdditionExpression): AdditionExpression = new AdditionExpression(e.y, e.x)

  // a(bv) = (ab)v
  def reassoc(scalar: Expression[U], e: MultiplicationExpression): MultiplicationExpression = {
    new MultiplicationExpression(new field.MultiplicationExpression(scalar, e.x), e.y)
  }

  // (ab)v = a(bv)
  def reassoc(e: field.MultiplicationExpression, t: T): MultiplicationExpression = {
    new MultiplicationExpression(e.x, new MultiplicationExpression(e.y, new UnaryExpression[T](t)))
  }

  // a(u + v) = au + av
  def distributeScalar(scalar: Expression[U], e: AdditionExpression): AdditionExpression = {
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
  def distributeVector(e: field.AdditionExpression, t: T): AdditionExpression = {
    new AdditionExpression(
      new MultiplicationExpression(e.x, new UnaryExpression[T](t)),
      new MultiplicationExpression(e.y, new UnaryExpression[T](t))
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
  // A vector space has a unique additive identity.
  // Suppose x is also an additive identity...
  def fn[T, U, F <: Field[U]](vs: VectorSpace[T, U, F], x: T): Unit = {
    // x = x + 0
    val expression = vs.addAdditiveIdentity(new UnaryExpression[T](x))

    // x + 0 = 0 + x
    val commutedExpression = vs.commute(expression)

    // 0 + x = 0
    val expression2 = vs.removeAdditiveIdentity(commutedExpression)

    // x = 0
    assert(expression2.evaluate() == vs.zero())
  }

  def main(args: Array[String]): Unit = {
  }
}