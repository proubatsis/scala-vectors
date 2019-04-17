package ca.panagiotis.scalavec

case class Vec3[T](x : T, y : T, z: T)(implicit  num: Numeric[T]) extends Vec[T, Vec3] {
  import num._
  override val components: List[T] = List(x, y, z)

  override def instantiate(components: List[T]): Vec3[T] = Vec3(components)
  override def instantiateDouble(components: List[Double]): Vec3[Double] = Vec3(components)

  def cross(that: Vec3[T]): Vec3[T] =
    Vec3((y * that.z) - (z * that.y), (z * that.x) - (x * that.z), (x * that.y) - (y * that.x))
}

object Vec3 {
  def apply[T](components: List[T])(implicit num: Numeric[T]) : Vec3[T] = components match {
    case List(x, y, z) => Vec3(x, y, z)
    case _ => zero
  }

  def zero[T](implicit num: Numeric[T]): Vec3[T] = Vec3(num.zero, num.zero, num.zero)

  def fromVec2[T](vec: Vec2[T], z: T)(implicit num: Numeric[T]): Vec3[T] = Vec3(vec.x, vec.y, z)
  def fromVec4[T](vec: Vec4[T])(implicit num: Numeric[T]): Vec3[T] = Vec3(vec.x, vec.y, vec.z)
}
