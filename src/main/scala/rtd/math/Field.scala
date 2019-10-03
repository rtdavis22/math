package rtd.math

trait Field[T] {
  class AdditionExpression(x: Expression[T], y: Expression[T]) extends BinaryExpression[T, T, T](x, y) {
    override def evaluate(): T = {
      Field.this.+(x.evaluate(), y.evaluate())
    }
  }
  class MultiplicationExpression(x: Expression[T], y: Expression[T]) extends BinaryExpression[T, T, T](x, y) {
    override def evaluate(): T = {
      Field.this.*(x.evaluate(), y.evaluate())
    }
  }

  def +(e1: T, e2: T): T
  def *(e1: T, e2: T): T
  def additivelyInvert(x: T): T
  def multiplicativelyInvert(x: T): T

  def additiveIdentity(): T
  def multiplicativeIdentity(): T

  def commute(e: AdditionExpression): AdditionExpression = new AdditionExpression(e.y, e.x)
  def commute(e: MultiplicationExpression): MultiplicationExpression = new MultiplicationExpression(e.y, e.x)

  // a + (b + c) = (a + b) + c
  def reassoc(t: T, e: AdditionExpression): AdditionExpression = {
    new AdditionExpression(new AdditionExpression(new UnaryExpression(t), e.x), e.y)
  }

  // (a + b) + c = a + (b + c)
  def reassoc(e: AdditionExpression, t: T): AdditionExpression = {
    new AdditionExpression(e.x, new AdditionExpression(e.y, new UnaryExpression(t)))
  }

  // a(bc) = (ab)c
  def reassoc(t: T, e: MultiplicationExpression): MultiplicationExpression = {
    new MultiplicationExpression(new MultiplicationExpression(new UnaryExpression(t), e.x), e.y)
  }

  // (ab)c = a(bc)
  def reassoc(e: MultiplicationExpression, t: T): MultiplicationExpression = {
    new MultiplicationExpression(e.x, new MultiplicationExpression(e.y, new UnaryExpression(t)))
  }

  // a(b + c) = ab + ac
  def distribute(x: T, e: AdditionExpression): AdditionExpression = {
    new AdditionExpression(
      new MultiplicationExpression(new UnaryExpression[T](x), e.x),
      new MultiplicationExpression(new UnaryExpression[T](x), e.y))
  }

  // ab + ac = a(b + c)
  def undistribute(e1: MultiplicationExpression, e2: MultiplicationExpression): MultiplicationExpression = {
    if (e1.x != e2.x) {
      throw new Exception("cannot undistribute")
    }
    new MultiplicationExpression(e1.x, new AdditionExpression(e1.y, e2.y))
  }
}
