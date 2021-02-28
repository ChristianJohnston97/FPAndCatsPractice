package data_structures

import cats.{Foldable, Monoid}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

}


object TreeFoldable extends Foldable[Tree] {
  override def foldLeft[A, B](as: Tree[A])(b: B)(f: (B, A) => B): B = as match {
    case Leaf(value) => f(b, value)
    case Branch(left, right) => foldLeft(right)(foldLeft(left)(b)(f))(f)
  }

  override def foldRight[A, B](as: Tree[A])(b: B)(f: (A, B) => B): B = ???

  override def foldMap[A, B](as: Tree[A])(f: A => B)(implicit m: Monoid[B]): B = ???
}