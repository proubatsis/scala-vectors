package ca.panagiotis.scalavec

trait Mat[T, V[T] <: Vec[T, V], F[X] <: Mat[X, V, F]] {
  def rows: List[List[T]]

  def instantiate(rows: List[List[T]]): F[T]
  def instantiateVector(row: List[T]): V[T]

  def +(other: F[T])(implicit num: Numeric[T]): F[T] = {
    def addRow(rowA: List[T], rowB: List[T]) =
      rowA.zip(rowB).map({ case (nA, nB) => num.plus(nA, nB) })

    instantiate(rows.zip(other.rows).map({ case (rowA, rowB) => addRow(rowA, rowB) }))
  }

  def -(other: F[T])(implicit num: Numeric[T]): F[T] = {
    def minusRow(rowA: List[T], rowB: List[T]) =
      rowA.zip(rowB).map({ case (nA, nB) => num.minus(nA, nB) })

    instantiate(rows.zip(other.rows).map({ case (rowA, rowB) => minusRow(rowA, rowB) }))
  }

  def *(f: T)(implicit num: Numeric[T]): F[T] = {
    def scaleRow(row: List[T]) = row.map(num.times(_, f))
    instantiate(rows.map(scaleRow))
  }

  def *(v: V[T])(implicit num: Numeric[T]): V[T] = {
    val vRows = rows.map(instantiateVector)
    val components = vRows.map(v.dot(_))
    instantiateVector(components)
  }

  def *(other: F[T])(implicit num: Numeric[T]): F[T] = {
    val vRows = rows.map(instantiateVector)
    val cols = other.transpose.rows.map(instantiateVector)

    val rowCols = vRows.map(r => (r, cols))
    instantiate(rowCols.map({ case (r, c) => c.map(r.dot(_)) }))
  }

  def transpose: F[T] = instantiate(rows.transpose)
}
