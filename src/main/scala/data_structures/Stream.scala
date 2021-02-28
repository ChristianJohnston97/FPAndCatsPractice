package data_structures

import scala.annotation.tailrec

/**
 * A non-empty stream consists of a head and a tail which are both non-strict
 * very similar to list data structure but lazy
 */
object Stream {
  sealed trait Stream[+A]
  case object Empty extends Stream[Nothing]
  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  /**
   * Smart constructor
   */
  def cons[A](h: => A, t: => Stream[A]): Stream[A] = {
    Cons(() => h, () => t)
  }

  /**
   * If we wish to examine of travserse the stream, we need to force these thunks explicitly
   */
  def headOption[A](stream: Stream[A]): Option[A] = stream match {
    case Empty => None
    case Cons(h, _) =>Some(h())
  }

  /**
   * convert stream to list recursively (not tail recursive)
   */
  def toList[A](stream: Stream[A]): List[A] = stream match {
    case Empty => List()
    case Cons(h, t) => h() :: toList(t())
  }

  /**
   * convert stream to list recursively (tail recursive)
   */
  def toListTailRecursive[A](stream: Stream[A]): List[A] = {
    @tailrec
    def innerDo(stream: Stream[A], acc: List[A]): List[A] =
      stream match {
        case Cons(h, t) => innerDo(t(), h() :: acc)
        case Empty => acc
      }
    innerDo(stream, List())
  }

  def takeN[A](stream: Stream[A], n: Int): Stream[A] = stream match {
    case Cons(h, t) if n > 1 => cons[A](h(), takeN(t(), n-1))
    case Cons(_, t) if n == 1 => takeN(t(), n-1)
    case Empty => Empty
  }

  def takeWhile[A](stream: Stream[A])(p: A => Boolean): Stream[A] = stream match {
    case Empty => Empty
    case Cons(h, t) if p(h()) => cons(h(), takeWhile(t())(p))
  }

  def forAll[A](stream: Stream[A], p: A => Boolean): Boolean = stream match {
    case Empty => true
    case Cons(h, t) if p(h()) => forAll(t(), p)
  }

}