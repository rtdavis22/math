package rtd.math

class SymmetricGroup[T](n: Int) extends Group[Permutation] {
  override def invert(p: Permutation): Permutation = p.invert()

  override def compose(p1: Permutation, p2: Permutation): Permutation = p1.compose(p2)

  override def identity(): Permutation = new Permutation(Map())
}

class Permutation(val m: Map[Int, Int]) {
  def invert(): Permutation = {
    new Permutation(m.map(_.swap))
  }

  // Simpler way to do this?
  def compose(p: Permutation): Permutation = {
    val newMap = collection.mutable.Map[Int, Int]()
    var toIgnore: Set[Int] = Set()
    for (k <- m.keys) {
      if (!p.m.contains(m(k))) {
        newMap(k) = m(k)
      } else {
        val v = p.m(m(k))
        if (v != k) {
          newMap(k) = v
        }
        toIgnore = toIgnore + m(k)
      }
    }
    for (k <- p.m.keys) {
      if (!toIgnore.contains(k)) {
        newMap(k) = p.m(k)
      }
    }
    new Permutation(newMap.toMap)
  }

  def map(i: Int): Int = {
    m.getOrElse(i, i)
  }
}

object Permutation {
  def apply(m: Map[Int, Int]): Permutation = {
    new Permutation(m)
  }
}