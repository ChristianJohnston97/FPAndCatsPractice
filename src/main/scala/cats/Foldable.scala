package cats

trait Foldable[F[_]] {

  def foldLeft[A, B](as: F[A])(b: B)(f: (B, A) => B): B
  def foldRight[A, B](as: F[A])(b: B)(f: (A, B) => B): B
  def foldMap[A, B](as: F[A])(f: A => B)(implicit m: Monoid[B]): B

  def toList[A](fa: F[A]): List[A] =
    foldLeft(fa)(List[A]())((b, a) => a::b)
}

