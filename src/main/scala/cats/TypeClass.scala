package cats

import cats.TypeClass.Monoid

/**
 * Example of using type classes
 * Enables ad-hoc polymorphism
 */
object TypeClass {

  trait Monoid[A] {
    def empty: A
    def combine(a: A, b: A): A // associative binary operation
  }

  object Monoid {
    implicit val intCombine: Monoid[Int] = new Monoid[Int] {
      override def empty: Int = 0
      override def combine(a: Int, b: Int): Int = a + b
    }

    /**
     * Provide a utility method on the companion object of the type class
     * that skirts the need to call implicitly everywhere.
     */
    object Monoid {
      def apply[A : Monoid]: Monoid[A] = implicitly[Monoid[A]]
    }
  }
}

object MonoidUse {

  import Monoid.Monoid

  //ok
  def combineAll[A](aList: List[A])(implicit monoid: Monoid[A]): A =
    aList.foldLeft(monoid.empty)(monoid.combine)

  // better
  def combineAll[A: Monoid](aList: List[A]): A =
    aList.foldLeft(implicitly[Monoid[A]].empty)(implicitly[Monoid[A]].combine)

  // best
  def combineAll[A: Monoid](aList: List[A]): A =
    aList.foldLeft(Monoid[A].empty)(Monoid[A].combine)
}