package rtd.math

import scala.reflect.{ClassTag, Manifest}

abstract class IntType{ val value:Int }

class ElementOfCoordinateSpace[T, I <: IntType](implicit manifest: Manifest[I], implicit var m: ClassTag[T]) {
  val length: Int = manifest.erasure.asInstanceOf[Class[I]].newInstance.value
  val array = new Array[T](length)


}

class CoordinateSpace[T, S <: IntType](field: Field[T])(implicit manifest: Manifest[S], m: ClassTag[T])
    extends VectorSpace[ElementOfCoordinateSpace[T, S], T](field) {
  override def +(t1: ElementOfCoordinateSpace[T, S], t2: ElementOfCoordinateSpace[T, S]): ElementOfCoordinateSpace[T, S] = {
    val s = new ElementOfCoordinateSpace[T, S]()
    for (i <- 0 until s.length) {
      s.array(i) = field.+(t1.array(i), t2.array(i))
    }
    s
  }

  override def *(a: T, t: ElementOfCoordinateSpace[T, S]): ElementOfCoordinateSpace[T, S] = {
    val s = new ElementOfCoordinateSpace[T, S]()
    for (i <- 0 until s.length) {
      s.array(i) = field.*(a, t.array(i))
    }
    s
  }

  override def additivelyInvert(t: ElementOfCoordinateSpace[T, S]): ElementOfCoordinateSpace[T, S] = {
    val s = new ElementOfCoordinateSpace[T, S]()
    for (i <- 0 until s.length) {
      s.array(i) = field.additivelyInvert(t.array(i))
    }
    s
  }

  override def additiveIdentity(): ElementOfCoordinateSpace[T, S] = {
    val s = new ElementOfCoordinateSpace[T, S]()
    for (i <- 0 until s.length) {
      s.array(i) = field.additiveIdentity()
    }
    s
  }
}

class RealVectorSpace[S <: IntType](implicit manifest: Manifest[S]) extends CoordinateSpace[ElementOfR, S](new  ℝ()) {}

object CoordinateSpace {
  def main(args: Array[String]): Unit = {
    class I10 extends IntType { val value = 10 }

    val vs = new RealVectorSpace[I10]()

    println(vs)
  }
}