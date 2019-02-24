package ca.panagiotis.scalavec

trait Vec[T, F[X] <: Vec[X, F]] {
  val components: List[T]

  def instantiate(components: List[T]): F[T]
  def instantiateDouble(components: List[Double]): F[Double]

  def +(other: F[T])(implicit num: Numeric[T]): F[T] =
    instantiate(components.zip(other.components).map({ case (cA, cB) => num.plus(cA, cB) }))

  def -(other: F[T])(implicit num: Numeric[T]): F[T] =
    instantiate(components.zip(other.components).map({ case (cA, cB) => num.minus(cA, cB) }))

  def *(f: T)(implicit num: Numeric[T]): F[T] =
    instantiate(components.map(num.times(_, f)))

  def length(implicit num: Numeric[T]): Double =
    Math.sqrt(num.toDouble(components.map(c => num.times(c, c)).sum))

  def dot(other: F[T])(implicit num: Numeric[T]): T =
    components.zip(other.components).map({ case (cA, cB) => num.times(cA, cB) }).sum

  def normalize(implicit num: Numeric[T]): F[Double] =
    instantiateDouble(components.map(c => num.toDouble(c) / length))
}
