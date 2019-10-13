package rtd.math

class Q extends Field[(Int, Int)] {
  override def +(a: (Int, Int), b: (Int, Int)): (Int, Int) = (a._1*b._2 + a._2*b._1, a._2*b._2)

  override def *(a: (Int, Int), b: (Int, Int)): (Int, Int) = (a._1*b._1, a._2*b._2)

  override def -(q: (Int, Int)): (Int, Int) = (-q._1, q._2)

  override def invert(q: (Int, Int)): (Int, Int) = (q._2, q._1)

  override def zero: (Int, Int) = (0, 0)

  override def one: (Int, Int) = (1, 1)
}