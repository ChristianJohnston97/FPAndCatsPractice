package cats

/**
  cats.Monoid type class, used to combine elements
  https://typelevel.org/cats/typeclasses/monoid.html
 */
trait Monoid[A] extends SemiGroup[A] {
  def empty: A
}

object Monoid {
  implicit val intCombine: Monoid[Int] = new Monoid[Int] {
    override def empty: Int = 0
    override def combine(a: Int, b: Int): Int = a + b
  }

  def productMonoid[A, B](a: Monoid[A], b: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
    override def empty: (A, B) = (a.empty, b.empty)
    override def combine(a: (A, B), b: (A, B)): (A, B) = ???
  }

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = {
    as.foldLeft(m.empty)((acc, a) => m.combine(f(a), acc))
  }

  // so we don't have to write implicitly[Monoid[A]] everywhere
  def apply[A: Monoid]: Monoid[A] = implicitly[Monoid[A]]


  //  Combining Type Classes and Type Enrichment
  implicit class MonoidOps[A](value: A) {
    def combine(b: A)(implicit monoid: Monoid[A]): A = monoid.combine(value, b)
  }
}

object MonoidInterface {
  def combineAll[A: Monoid](as: List[A]): A =
    as.foldLeft(Monoid[A].empty)(Monoid[A].combine)


  def associativeLaw[A](x: A, y: A, z: A)
                       (implicit m: Monoid[A]): Boolean =
    m.combine(x, m.combine(y, z)) == m.combine(m.combine(x, y), z)

  def identityLaw[A](x: A)
                    (implicit m: Monoid[A]): Boolean =
    (m.combine(x, m.empty) == x) && (m.combine(m.empty, x) == x)
}