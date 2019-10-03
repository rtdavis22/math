package rtd.math

abstract class Expression[T] {
  def evaluate(): T
}

abstract class BinaryExpression[T, U, V](val x: Expression[T], val y: Expression[U]) extends Expression[V] {
}

class UnaryExpression[T](val x: T) extends Expression[T] {
  def evaluate(): T = x
}