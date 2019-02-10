package ca.panagiotis.scalavec

object Vec {
  def length[T](components: List[T])(implicit num: Numeric[T]): Double =
    Math.sqrt(num.toDouble(components.map(c => num.times(c, c)).sum))

  def dot[T](componentsA: List[T], componentsB: List[T])(implicit num: Numeric[T]): T =
    componentsA.zip(componentsB).map({ case (cA, cB) => num.times(cA, cB) }).sum

  def normalize[T](components: List[T])(implicit num: Numeric[T]): List[Double] =
    components.map(c => num.toDouble(c) / length(components))
}
