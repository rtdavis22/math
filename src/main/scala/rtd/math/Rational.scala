package rtd.math

class ElementOfQ(val num: Int, val den: Int) {}

class Q extends Field[ElementOfQ] {
  override def +(a: ElementOfQ, b: ElementOfQ): ElementOfQ = {
    new ElementOfQ(a.num*b.den + a.den*b.num, a.den*b.den)
  }

  override def *(a: ElementOfQ, b: ElementOfQ): ElementOfQ = {
    new ElementOfQ(a.num*b.num, a.den*b.den)
  }

  override def additivelyInvert(x: ElementOfQ): ElementOfQ = {
    new ElementOfQ(-x.num, x.den)
  }

  override def multiplicativelyInvert(x: ElementOfQ): ElementOfQ = {
    new ElementOfQ(x.den, x.num)
  }

  override def additiveIdentity(): ElementOfQ = {
    new ElementOfQ(0, 0)
  }

  override def multiplicativeIdentity(): ElementOfQ = {
    new ElementOfQ(1, 1)
  }
}