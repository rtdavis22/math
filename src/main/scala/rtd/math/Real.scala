package rtd.math

import scala.reflect.Manifest

case class R(var v: Double) {
  override def toString: String = v.toString
}

class Reals extends Field[R] {
  override def +(x: R, y: R): R = R(x.v + y.v)

  override def *(x: R, y: R): R = R(x.v * y.v)

  override def -(x: R): R = R(-x.v)

  override def invert(x: R): R = {
    assert(x != zero)
    R(1.0/x.v)
  }

  override def zero: R = R(0.0)

  override def one: R = R(1.0)
}

class RealVectorSpace[S <: SizeType](implicit manifest: Manifest[S]) extends CoordinateSpace[R, S](new  Reals) {}
