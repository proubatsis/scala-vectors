package ca.panagiotis.scalavec

case class Vec4[T](x : T, y : T, z: T, w: T)(implicit  num: Numeric[T]){
  private val components = List(x, y, z, w)

  def +(that: Vec4[T]) = Vec4(Vec.plus(components, that.components))
  def -(that: Vec4[T]) = Vec4(Vec.minus(components, that.components))
  def *(f: T) = Vec4(Vec.scale(components, f))

  def dot(that: Vec4[T]): T = Vec.dot(components, that.components)
  def normalize: Vec4[Double] = Vec4(Vec.normalize(components))
  def length: Double = Vec.length(components)
}

object Vec4 {
  def apply[T](components: List[T])(implicit num: Numeric[T]) : Vec4[T] = components match {
    case List(w, x, y, z) => Vec4(w, x, y, z)
    case _ => zero
  }

  def zero[T](implicit num: Numeric[T]): Vec4[T] = Vec4(num.zero, num.zero, num.zero, num.zero)
}
