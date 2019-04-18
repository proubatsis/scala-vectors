package io.github.proubatsis

case class Vec4[T](x : T, y : T, z: T, w: T)(implicit  num: Numeric[T]) extends Vec[T, Vec4] {
  override val components: List[T] = List(x, y, z, w)

  override def instantiate(components: List[T]): Vec4[T] = Vec4(components)
  override def instantiateDouble(components: List[Double]): Vec4[Double] = Vec4(components)
}

object Vec4 {
  def apply[T](components: List[T])(implicit num: Numeric[T]) : Vec4[T] = components match {
    case List(w, x, y, z) => Vec4(w, x, y, z)
    case _ => zero
  }

  def zero[T](implicit num: Numeric[T]): Vec4[T] = Vec4(num.zero, num.zero, num.zero, num.zero)

  def fromVec3[T](vec: Vec3[T], isPositionVec: Boolean)(implicit num: Numeric[T]): Vec4[T] =
    Vec4(vec.x, vec.y, vec.z, if (isPositionVec) num.one else num.zero)
}
