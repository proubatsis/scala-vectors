package io.github.proubatsis

case class Mat3[T](a00: T, a01: T, a02: T,
                   a10: T, a11: T, a12: T,
                   a20: T, a21: T, a22: T)(implicit num: Numeric[T]) extends Mat[T, Vec3, Mat3] {

  val rows = List(
    List(a00, a01, a02),
    List(a10, a11, a12),
    List(a20, a21, a22)
  )

  override def instantiate(rows: List[List[T]]): Mat3[T] = Mat3(rows)
  override def instantiateVector(row: List[T]): Vec3[T] = Vec3(row)
}

object Mat3 {
  def apply[T](items: List[List[T]])(implicit num: Numeric[T]): Mat3[T] = items match {
    case List(
      List(a00, a01, a02),
      List(a10, a11, a12),
      List(a20, a21, a22)
    ) => Mat3(a00, a01, a02, a10, a11, a12, a20, a21, a22)
    case _ => zero
  }

  def zero[T](implicit num: Numeric[T]): Mat3[T] =
    Mat3(
      num.zero, num.zero, num.zero,
      num.zero, num.zero, num.zero,
      num.zero, num.zero, num.zero)

  def identity[T](implicit num: Numeric[T]): Mat3[T] =
    Mat3(
      num.one, num.zero, num.zero,
      num.zero, num.one, num.zero,
      num.zero, num.zero, num.one)
}
