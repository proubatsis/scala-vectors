package io.github.proubatsis

case class Mat2[T](a00: T, a01: T,
                   a10: T, a11: T)(implicit num: Numeric[T]) extends Mat[T, Vec2, Mat2] {

  val rows = List(
    List(a00, a01),
    List(a10, a11)
  )

  override def instantiate(rows: List[List[T]]): Mat2[T] = Mat2(rows)
  override def instantiateVector(row: List[T]): Vec2[T] = Vec2(row)
}

object Mat2 {
  def apply[T](items: List[List[T]])(implicit num: Numeric[T]): Mat2[T] = items match {
    case List(
      List(a00, a01),
      List(a10, a11)
    ) => Mat2(a00, a01, a10, a11)
    case _ => zero
  }

  def zero[T](implicit num: Numeric[T]): Mat2[T] =
    Mat2(
      num.zero, num.zero,
      num.zero, num.zero)

  def identity[T](implicit num: Numeric[T]): Mat2[T] =
    Mat2(
      num.one, num.zero,
      num.zero, num.one)
}
