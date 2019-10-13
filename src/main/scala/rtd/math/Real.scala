package rtd.math

import scala.reflect.Manifest

class R(var v: Double) {
  override def toString: String = v.toString
}

class ℝ extends Field[R] {
  override def +(x: R, y: R): R = new R(x.v + y.v)

  override def *(x: R, y: R): R = new R(x.v * y.v)

  override def -(x: R): R = new R(-x.v)

  override def invert(x: R): R = {
    assert(x != zero)
    new R(1.0/x.v)
  }

  override def zero: R = new R(0.0)

  override def one: R = new R(1.0)
}

class RealVectorSpace[S <: SizeType](implicit manifest: Manifest[S]) extends CoordinateSpace[R, S](new  ℝ()) {}
