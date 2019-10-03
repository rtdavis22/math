package rtd.math

abstract class VectorSpace[T, U](field: Field[U]) {
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

  def +(t1: T, t2: T): T
  def *(a: U, x: T): T

  def additivelyInvert(t: T): T

  def additiveIdentity(): T

  // 1v = v
  def removeIdentity(e: MultiplicationExpression): Expression[T] = {
    if (e.x != field.multiplicativeIdentity()) {
      throw new Exception("cannot remove identity")
    }
    e.y
  }

  // v = 1v
  def addIdentity(e: UnaryExpression[T]): MultiplicationExpression = {
    new MultiplicationExpression(new UnaryExpression[U](field.multiplicativeIdentity()), e)
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