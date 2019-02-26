package ca.panagiotis.scalavec

case class Mat4[T](a00: T, a01: T, a02: T, a03: T,
                   a10: T, a11: T, a12: T, a13: T,
                   a20: T, a21: T, a22: T, a23: T,
                   a30: T, a31: T, a32: T, a33: T)(implicit num: Numeric[T]) {

  private val items = List(
    List(a00, a01, a02, a03),
    List(a10, a11, a12, a13),
    List(a20, a21, a22, a23),
    List(a30, a31, a32, a33)
  )

  def +(other: Mat4[T]): Mat4[T] = {
    def addRow(rowA: List[T], rowB: List[T]) =
      rowA.zip(rowB).map({ case (nA, nB) => num.plus(nA, nB) })

    Mat4(items.zip(other.items).map({ case (rowA, rowB) => addRow(rowA, rowB) }))
  }

  def -(other: Mat4[T]): Mat4[T] = {
    def minusRow(rowA: List[T], rowB: List[T]) =
      rowA.zip(rowB).map({ case (nA, nB) => num.minus(nA, nB) })

    Mat4(items.zip(other.items).map({ case (rowA, rowB) => minusRow(rowA, rowB) }))
  }

  def *(f: T): Mat4[T] = {
    def scaleRow(row: List[T]) = row.map(num.times(_, f))
    Mat4(items.map(scaleRow))
  }

  def *(v: Vec4[T]): Vec4[T] = {
    val rows = items.map(r => Vec4(r))
    val components = rows.map(v.dot(_))
    Vec4(components)
  }

  def *(other: Mat4[T]): Mat4[T] = {
    val rows = items.map(r => Vec4(r))
    val cols = other.transpose.items.map(c => Vec4(c))

    val rowCols = rows.map(r => (r, cols))
    Mat4(rowCols.map({ case (r, c) => c.map(r.dot(_)) }))
  }

  def transpose: Mat4[T] = Mat4(items.transpose)
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
