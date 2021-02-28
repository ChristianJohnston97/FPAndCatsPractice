package cats


trait IO[A] {
  def flatMap[B](f: A => IO[B]): IO[B] = FlatMap(this, f)
  def map[B](f: A => B): IO[B] = flatMap(f andThen (Return(_)))
}


/**
 * 3 data constructors representing the 3 different types of control flow that we want the interpreter of this
 * data type to support.
 * Return represents an IO action that has finished
 * Suspend means that we want to execute some effect to produce a result
 * FlatMap lets us extend or continue an existing computation by using the result of the preceding computation
 */
case class Return[A](a: A) extends IO[A]
case class FlatMap[A, B](sub: IO[A], k: A => IO[B]) extends IO[B]

