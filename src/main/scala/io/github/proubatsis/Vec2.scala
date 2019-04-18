package io.github.proubatsis

case class Vec2[T](x : T, y : T)(implicit  num: Numeric[T]) extends Vec[T, Vec2] {
  override val components: List[T] = List(x, y)

  override def instantiate(components: List[T]): Vec2[T] = Vec2(components)
  override def instantiateDouble(components: List[Double]): Vec2[Double] = Vec2(components)
}

object Vec2 {
  def apply[T](components: List[T])(implicit num: Numeric[T]) : Vec2[T] = components match {
    case List(x, y) => Vec2(x, y)
    case _ => zero
  }

  def zero[T](implicit num: Numeric[T]): Vec2[T] = Vec2(num.zero, num.zero)
}
