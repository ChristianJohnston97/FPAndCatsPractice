package cats

/**
 * A free monad is a construction that allows us to build a very simple Monad from any functor.
 * - takes a functor
 * - adds pure
 * - adds flatMap
 */


/**
 * Free is a recursive structure. It uses A in F[A] as the recursion “carrier” with a terminal element Pure.
 */
sealed abstract class Free[F[_], A]

/**
 * Pure builds a Free instance from an A value (it reifies the pure function)
 */
case class Pure[F[_], A](a: A) extends Free[F, A]

/**
 * Suspend builds a new Free by applying F to a previous Free (it reifies the flatMap function)
 */
case class Suspend[F[_], A](a: F[Free[F, A]]) extends Free[F, A]

