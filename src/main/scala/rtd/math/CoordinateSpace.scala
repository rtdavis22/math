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
    t1
  }

  override def *(a: T, t: ElementOfCoordinateSpace[T, S]): ElementOfCoordinateSpace[T, S] = {
    t
  }

  override def additivelyInvert(t: ElementOfCoordinateSpace[T, S]): ElementOfCoordinateSpace[T, S] = {
    t
  }

  override def additiveIdentity(): ElementOfCoordinateSpace[T, S] = {
    new ElementOfCoordinateSpace[T, S]()
  }
}

object CoordinateSpace {
  def main(args: Array[String]): Unit = {
    class I10 extends IntType { val value = 10 }

    val cs = new CoordinateSpace[ElementOfR, I10](new ℝ())

    println(cs)
  }
}