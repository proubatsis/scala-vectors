package ca.panagiotis.scalavec

case class Mat4[T](a00: T, a01: T, a02: T, a03: T,
                   a10: T, a11: T, a12: T, a13: T,
                   a20: T, a21: T, a22: T, a23: T,
                   a30: T, a31: T, a32: T, a33: T)(implicit num: Numeric[T]) extends Mat[T, Vec4, Mat4] {

  val rows = List(
    List(a00, a01, a02, a03),
    List(a10, a11, a12, a13),
    List(a20, a21, a22, a23),
    List(a30, a31, a32, a33)
  )

  override def instantiate(rows: List[List[T]]): Mat4[T] = Mat4(rows)
  override def instantiateVector(row: List[T]): Vec4[T] = Vec4(row)
}

object Mat4 {
  def apply[T](items: List[List[T]])(implicit num: Numeric[T]): Mat4[T] = items match {
    case List(
      List(a00, a01, a02, a03),
      List(a10, a11, a12, a13),
      List(a20, a21, a22, a23),
      List(a30, a31, a32, a33)
    ) => Mat4(a00, a01, a02, a03, a10, a11, a12, a13, a20, a21, a22, a23, a30, a31, a32, a33)
    case _ => zero
  }

  def zero[T](implicit num: Numeric[T]): Mat4[T] =
    Mat4(
      num.zero, num.zero, num.zero, num.zero,
      num.zero, num.zero, num.zero, num.zero,
      num.zero, num.zero, num.zero, num.zero,
      num.zero, num.zero, num.zero, num.zero)

  def identity[T](implicit num: Numeric[T]): Mat4[T] =
    Mat4(
      num.one, num.zero, num.zero, num.zero,
      num.zero, num.one, num.zero, num.zero,
      num.zero, num.zero, num.one, num.zero,
      num.zero, num.zero, num.zero, num.one)
}
