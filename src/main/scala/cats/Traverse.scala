package cats


/**
 * Difference between traverse and foldMap is that the former preserves the original structure
 * whereas foldMap discards the structure and replaces it with the operations of a monoid.
 *
 * Traverse extends functor illustrating that the traverse function is a generalisation of map.
 */
trait Traverse[F[_]] extends Functor[F] {
  def traverse[G[_]: Applicative, A, B](as: F[A])(f: A => G[B]): G[F[B]]
  def sequence[G[_]: Applicative, A](as: F[G[A]]): G[F[A]]
  override def map[A, B](fa: F[A])(f: A => B): F[B] = ???
}

object Traverse {

  object ListTraverse extends Traverse[List] {
    override def traverse[G[_] : Applicative, A, B](as: List[A])(f: A => G[B]): G[List[B]] = {
      as.foldLeft(implicitly[Applicative[G]].pure(List[B]()))((acc, a) => {
        implicitly[Applicative[G]].map2(acc, f(a))((lb, b) => b::lb)
      })
    }

    override def sequence[G[_] : Applicative, A](as: List[G[A]]): G[List[A]] =
      traverse(as)(identity)
  }

  object OptionTraverse extends Traverse[Option] {
    override def traverse[G[_] : Applicative, A, B](as: Option[A])(f: A => G[B]): G[Option[B]] = as match {
      case Some(a) => implicitly[Applicative[G]].map(f(a))(b => Some(b))
      case None => implicitly[Applicative[G]].pure(None[B])
    }

    override def sequence[G[_] : Applicative, A](as: Option[G[A]]): G[Option[A]] =
      traverse(as)(identity)
  }
}
