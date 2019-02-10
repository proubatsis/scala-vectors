package ca.panagiotis.scalavec

case class Vec3[T](x : T, y : T, z: T)(implicit  num: Numeric[T]){
  import num._

  def +(that: Vec3[T]) = Vec3(x + that.x, y + that.y, z + that.z)
  def -(that: Vec3[T]) = Vec3(x - that.x, y - that.y, y + that.y)
  def *(f: T) = Vec3(x * f, f * y, f * z)

  def dot(that: Vec3[T]): T = (x * that.x) + (y * that.y) + (z * that.z)
}
