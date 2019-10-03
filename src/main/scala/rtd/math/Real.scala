package rtd.math

class ElementOfR(var v: Double) {}

class ℝ extends Field[ElementOfR] {
  override def +(e1: ElementOfR, e2: ElementOfR): ElementOfR = {
    new ElementOfR(e1.v + e2.v)
  }

  override def *(e1: ElementOfR, e2: ElementOfR): ElementOfR = {
    new ElementOfR(e1.v * e2.v)
  }

  override def additivelyInvert(x: ElementOfR): ElementOfR = {
    new ElementOfR(-x.v)
  }

  override def multiplicativelyInvert(x: ElementOfR): ElementOfR = {
    if (x == multiplicativeIdentity) {
      throw new Exception("cannot invert the multiplicative identity")
    }
    new ElementOfR(1.0/x.v)
  }

  override def additiveIdentity(): ElementOfR = {
    new ElementOfR(0.0)
  }

  override def multiplicativeIdentity(): ElementOfR = {
    new ElementOfR(1.0)
  }
}
