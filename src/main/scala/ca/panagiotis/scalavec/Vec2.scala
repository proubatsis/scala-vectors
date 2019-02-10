package ca.panagiotis.scalavec

case class Vec2[T](x : T, y : T)(implicit  num: Numeric[T]){
  private val components = List(x, y)

  def +(that: Vec2[T]) = Vec2(Vec.plus(components, that.components))
  def -(that: Vec2[T]) = Vec2(Vec.minus(components, that.components))
  def *(f: T) = Vec2(Vec.scale(components, f))

  def dot(that: Vec2[T]): T = Vec.dot(components, that.components)
  def normalize: Vec2[Double] = Vec2(Vec.normalize(components))
  def length: Double = Vec.length(components)
}

object Vec2 {
  def apply[T](components: List[T])(implicit num: Numeric[T]) : Vec2[T] = components match {
    case List(x, y) => Vec2(x, y)
    case _ => zero
  }

  def zero[T](implicit num: Numeric[T]): Vec2[T] = Vec2(num.zero, num.zero)
}
