package rtd.math

// The vector space of functions from a set S to a field with elements in T.
class Functions[S, T, F <: Field[T]](field: F) extends VectorSpace[S => T, T, F](field) {
  // (f + g)(x) = f(x) + g(x)
  override def +(f: S => T, g: S => T): S => T = s => field.+(f(s), g(s))

  // (cf)(x) = cf(x)
  override def *(c: T, f: S => T): S => T = s => field.*(c, f(s))

  // (-f)(x) = -f(x)
  override def -(f: S => T): S => T = s => field.-(f(s))

  // 0(x) = 0
  override def zero(): S => T = _ => field.zero()
}

object Functions {
  def main(args: Array[String]): Unit = {
    val vs2 = new Functions[(Int, Int), R, ℝ](new  ℝ())
    val func = (q: (Int, Int)) => new R(q._1 + q._2)
    val f2 = vs2.+(func, func)
    val res = f2((5, 3))

    println(res)
  }
}