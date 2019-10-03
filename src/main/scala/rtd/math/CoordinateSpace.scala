package rtd.math

// TODO: Remove assumption of Reals
class ElementOfRn {
}

class Rn extends VectorSpace[ElementOfRn, ElementOfR](new ℝ()) {
  override def +(t1: ElementOfRn, t2: ElementOfRn): ElementOfRn = {
    t1
  }

  override def *(a: ElementOfR, t: ElementOfRn): ElementOfRn = {
    t
  }

  override def additivelyInvert(t: ElementOfRn): ElementOfRn = {
    t
  }

  override def additiveIdentity(): ElementOfRn = {
    new ElementOfRn()
  }

  override def multiplicativeIdentity(): ElementOfRn = {
    new ElementOfRn()
  }
}
