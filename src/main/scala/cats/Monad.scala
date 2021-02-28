package cats

/**
 * A monad is a mechanism for sequencing computations.
 * Laws:
 * Le􏰄ft identity : pure(a).flatMap(func) == func(a)
 * Right identity: m.flatMap(pure) == m
 * Associativity: m.flatMap(f).flatMap(g) == m.flatMap(x => f(x).flatMap(g))
 *
 */
trait Monad[F[_]] extends Functor[F] with Applicative[F] {

  def flatMap[A, B](value: F[A])(func: A => F[B]): F[B]

  def tailRecM[A, B](a: A)(f: A => F[Either[A, B]]): F[B]

  // map can be defined in terms of flatmap and pure (from applicative)
  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)(x => pure(f(x)))

  override def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {
    flatMap(fa)(a => map(fb) (b => f(a, b)))
  }

  override def sequence[A](fa: List[F[A]]): F[List[A]] =
    fa.foldLeft(pure(List[A]()))((acc, a) => map2(a, acc)(_ :: _))

  override def traverse[A, B](fa: List[A])(f: A => F[B]): F[List[B]] =
    fa.foldLeft(pure(List[B]()))((acc, a) => map2(f(a), acc)(_ :: _))


  def replicate[A](fa: F[A], n: Int): F[List[A]] =
    if(n <= 0)
      pure(List[A]())
    else map2(fa, replicate(fa, n-1))(_ :: _)


  override def product[A, B](ma: F[A], mb: F[B]): F[(A, B)] =
    map2(ma, mb)((_, _))


  def filter[A](la: List[A])(f: A => Boolean): F[List[A]] =
    la.foldLeft(pure(List[A]()))((acc, a) => {
      if(f(a))
        map(acc)(list => a :: list)
      else
        acc
    })

  def filterM[A](la: List[A])(fa: A => F[Boolean]): F[List[A]] = la match {
    case ::(head, next) => flatMap(fa(head))(b => {
      if(!b)
        filterM(next)(fa)
      else
        filterM(head::next)(fa)
    })
    case Nil => pure(List[A]())
  }

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(b => g(b))


}
