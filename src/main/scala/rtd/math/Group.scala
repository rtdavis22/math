package rtd.math

abstract class Group[T] {
  def compose(t1: T, t2: T): T

  def invert(t: T): T

  def identity(): T

  // Exponentiation through repeated squaring. Run-time is O((log p)^3).
  def pow(t: T, n: Int): T = {
    var p: Int = n
    var result = identity()
    var curPow = t
    if (p < 0) {
      curPow = invert(t)
      p *= -1
    }
    while (p > 0) {
      if ((p & 1) == 1) {
        result = compose(result, curPow)
      }
      curPow = compose(curPow, curPow)
      p >>= 1
    }
    result
  }
}