package cats

/**
 * ApplicativeError extends Applicative to provide handling for types that represent the quality of an exception or an error, for example, Either[E, A]
 *
 * raiseError will generate the specific error type depending on what F[_] represents.
 * If F[_] is an Either, then ae.raiseError will return Left. If F[_] represents a Validation, then ae.raiseError will return Invalid
 */
trait MyApplicativeError[F[_], E] extends Applicative[F] {
  def raiseError[A](e: E): F[A]
  def handleErrorWith[A](fa: F[A])(f: E => F[A]): F[A]
  def handleError[A](fa: F[A])(f: E => A): F[A]
  def attempt[A](fa: F[A]): F[Either[E, A]]
}

object MyApplicativeErrorInstances {

  implicit def myApplicativeStringError[F[_]]: MyApplicativeError[F, String] = new MyApplicativeError[F, String] {
    override def raiseError[A](e: String): F[A] = ???
    override def handleErrorWith[A](fa: F[A])(f: String => F[A]): F[A] = ???
    override def handleError[A](fa: F[A])(f: String => A): F[A] = ???
    override def attempt[A](fa: F[A]): F[Either[String, A]] = ???
    override def pure[A](a: A): F[A] = ???
    override def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = ???
    override def ap[A, B](ff: F[A => B])(fa: F[A]): F[B] = ???
    override def map[A, B](a: F[A])(f: A => B): F[B] = ???
  }
}


object MyApplicativeErrorTesting {

  import MyApplicativeErrorInstances._

  def attemptDivideApplicativeError[F[_]: MyApplicativeError](x: Int, y: Int): F[Int] = {
    val ae = implicitly[MyApplicativeError[F, String]]
    if (y == 0) ae.raiseError("divisor is error")
    else ae.pure(x/y)
  }


  val f: Either[String, Int] = attemptDivideApplicativeError(30, 10)
}

