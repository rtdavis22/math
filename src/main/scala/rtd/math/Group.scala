package rtd.math

abstract class Group[T] {
  def compose(t1: T, t2: T): T

  def invert(t: T): T

  def identity(): T
}