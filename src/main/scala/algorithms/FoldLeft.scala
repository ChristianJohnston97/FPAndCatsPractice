package algorithms

import cats.Monad

import scala.annotation.tailrec

/**
 * The fold method for a List takes two argument the start value and a function.
 * This function also takes two arguments; the accumulated value and the current item in the list.
 * Fold left is stack safe
 *
 * Lots of examples: http://oldfashionedsoftware.com/2009/07/30/lots-and-lots-of-foldleft-examples/
 */
object FoldLeft {


  def foldLeft[A, B](initial: B)(op: (B, A) => B): B = ???


  def sum(list: List[Int]): Int = list.foldLeft(0)((acc, elem) => acc + elem)

  def factorial(n: Long): Long = {
    @tailrec
    def factorialAccumulator(acc: Long, n: Long): Long = {
      if (n == 0) acc
      else factorialAccumulator(n*acc, n-1)
    }
    factorialAccumulator(1, n)
  }


  def length[A](list: List[A]): Int =
    list.foldLeft(0)((b, _) => b+1)

  def last[A](list: List[A]): A =
    list.foldLeft(list.head)((_, a) => a)

  def get[A](list: List[A], idx: Int): A =
    list.foldLeft((list.head, 0))((valWithIndex, a) => {
      if(valWithIndex._2 == idx) valWithIndex
      else (a, valWithIndex._2+1)
    }) match {
      case (result, index) if idx == index => result
      case _ => throw new Exception("Bad index")
    }

  def reverse[A](as: List[A]): List[A] =
    as.foldLeft(List[A]())((r, c) => c :: r)

  def unique[A](as: List[A]): List[A] =
    reverse(as.foldLeft(List[A]())((r, c) => {
      if(r.contains(c))
        r
      else c :: r
    }))

  def insertionSort[A](list: List[A])(implicit or: A => Ordered[A]): List[A] =
    list.foldLeft(List[A]()) { (r,c) =>
      val (front, back) = r.span(_ < c)
      front ::: c :: back
    }

  /**
   * flatmap ensures sequential
   */
  def sequentialTraverse[F[_]: Monad, A, B](list: List[A])(f: A => F[B]): F[List[B]] =
    list.foldLeft(implicitly[Monad[F]].pure(List[B]()))((acc, a) => {
      implicitly[Monad[F]].flatMap(acc)(bs => implicitly[Monad[F]].map(f(a))(b => b :: bs))
    })





}
