package cats

/**
 * Cats has a NonEmptyList data type that has an implementation of Semigroup but no implementation of cats.Monoid.
 */
trait SemiGroup[A] {

  /**
   * an associative binary operation
   * combine(x, combine(y, z)) = combine(combine(x, y), z)
   */
  def combine(a: A, b: A): A
}
