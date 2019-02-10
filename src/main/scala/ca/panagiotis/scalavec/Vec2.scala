package ca.panagiotis.scalavec

case class Vec2[T](x : T, y : T)(implicit  num: Numeric[T]){
  import num._

  def +(that: Vec2[T]) = Vec2(x + that.x, y + that.y)
  def -(that: Vec2[T]) = Vec2(x - that.x, y - that.y)
  def *(f: T) = Vec2(x * f, y * f)

  def dot(that: Vec2[T]): T = (x * that.x) + (y * that.y)
}
