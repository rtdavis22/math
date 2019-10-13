package rtd.math

import scala.reflect.{ClassTag, Manifest}

abstract class SizeType {
  val value: Int
}

class Coordinate[T, +I <: SizeType](v: T)(implicit manifest: Manifest[I], implicit var m: ClassTag[T]) {
  val length: Int = manifest.erasure.asInstanceOf[Class[I]].newInstance.value
  val array: Array[T] = Array.fill[T](length)(v)

  override def toString: String = array.toSeq.toString
}

class CoordinateSpace[T, S <: SizeType](override val field: Field[T])(implicit manifest: Manifest[S], m: ClassTag[T])
    extends VectorSpace[Coordinate[T, S], T] {
  override def +(t1: Coordinate[T, S], t2: Coordinate[T, S]): Coordinate[T, S] = {
    val s = zero
    for (i <- 0 until s.length) {
      s.array(i) = field.+(t1.array(i), t2.array(i))
    }
    s
  }

  override def *(a: T, t: Coordinate[T, S]): Coordinate[T, S] = {
    val s = zero
    for (i <- 0 until s.length) {
      s.array(i) = field.*(a, t.array(i))
    }
    s
  }

  override def -(t: Coordinate[T, S]): Coordinate[T, S] = {
    val s = zero
    for (i <- 0 until s.length) {
      s.array(i) = field.-(t.array(i))
    }
    s
  }

  override def zero: Coordinate[T, S] = {
    new Coordinate[T, S](field.zero)
  }

  def standardBasis(): List[Coordinate[T, S]] = {
    var basis = List[Coordinate[T, S]]()
    // Better way to get length?
    val length = new Coordinate[T, S](field.zero).length
    for (i <- 0 until length) {
      val c = new Coordinate[T, S](field.zero)
      c.array(i) = field.one
      basis = basis :+ c
    }
    basis
  }
}

class I3 extends SizeType { val value = 3 }
class I4 extends SizeType { val value = 4 }
