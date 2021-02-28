package cats

/**
 * We can think of Functor's map method as “appending” a transformation to a chain
 * Functor instances must obey two rules:
 * - composition: f(g(a)) == g(f(a))
 * - identity: fa.map(x => x) == f(a)
 */
trait Functor[F[_]] {
  def map[A, B](a: F[A])(f: A => B): F[B]

  // lifts a function into the effectful function
  def lift[A, B](f: A => B): F[A] => F[B] =
    fa => map(fa)(f)
}

object FunctorInstances {
  implicit val functorForOption: Functor[Option] = new Functor[Option] {
    def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa match {
      case None    => None
      case Some(a) => Some(f(a))
    }
  }
}


/*
 Functor Notes:
 - We can view Functor as the ability to work with a single effect. We can apply a pure function to a single effectful value without needing to “leave” the effect.
 - Functors compose: if F and G have Functor instances, then so does F[G[_]].

 */