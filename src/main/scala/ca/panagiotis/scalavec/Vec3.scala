package ca.panagiotis.scalavec

case class Vec3[T](x : T, y : T, z: T)(implicit  num: Numeric[T]){
  import num._
  private val components = List(x, y, z)

  def +(that: Vec3[T]) = Vec3(x + that.x, y + that.y, z + that.z)
  def -(that: Vec3[T]) = Vec3(x - that.x, y - that.y, y + that.y)
  def *(f: T) = Vec3(x * f, f * y, f * z)

  def dot(that: Vec3[T]): T = Vec.dot(components, that.components)
  def normalize: Vec3[Double] = Vec3(Vec.normalize(components))
  def length: Double = Vec.length(components)
}

object Vec3 {
  def apply[T](components: List[T])(implicit num: Numeric[T]) : Vec3[T] = components match {
    case List(x, y, z) => Vec3(x, y, z)
    case _ => zero
  }

  def zero[T](implicit num: Numeric[T]): Vec3[T] = Vec3(num.zero, num.zero, num.zero)
}
