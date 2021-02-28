package cats

/**
 * Applicative type class extends the functor type class with the ap and pure methods
 * Applicative encodes working with multiple independent effects.
 * Between product and map, we can take multiple separate effectful values and compose them.
 * Applicatives compose. If F and G have Applicative instances, then so does `F[G[X]]`
 *
 * Applicative is less powerful than the Monad (can't sequence computation) but more general
 * All applicatives are functors
 */
trait Applicative[F[_]] extends Functor[F] {

  def pure[A](a: A): F[A]

  def product[A,B](fa: F[A], fb: F[B]): F[(A,B)] =
    map2(fa, fb)((_, _))

  /**
   * Equivalent of map and product
   */
  def ap[A, B](ff: F[A => B])(fa: F[A]): F[B] = ???


  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    map(product(fa, fb))(fab => f(fab._1, fab._2))

  /**
   * map2 is more general than map
   */
  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    map2(fa, pure(()))((a, _) => f(a))

  def map3[A, B, C](fa: F[A], fb: F[B], fc: F[C]): F[(A, B, C)] = {
    val fabc = product(product(fa, fb), fc)
    map(fabc) {
      case ((a, b), c) => (a, b, c)
    }
  }

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldLeft(pure(List[B]()))((acc, a) => map2(acc, f(a))((a, b) => b :: a))

  def sequence[A](as: List[F[A]]): F[List[A]] =
    traverse(as)(fa => fa)

  def replicate[A](n: Int, fa: F[A]): F[List[A]] =
    if(n <= 0) pure(List[A]())
    else replicate(n-1, fa)

  def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] =
    ofa.foldLeft(pure(Map[K, V]()))((acc, a) => {
      a match {
        case (k, fv) => map2(acc, fv)((k, v) => Map(k -> v))
      }
    })


}

