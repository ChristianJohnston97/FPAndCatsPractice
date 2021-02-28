package data_structures

/**
 * Expression of parallel computation
 */
trait Par[F[_]] {

  def unit[A](a: A): Par[A]

  /**
   * Combines the result of two parallel computations with a binary function
   */
  def map2[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C]

  /**
   * Marks a computation for concurrent evaluation
   */
  def fork[A](a: => Par[A]): Par[A]

  /**
   * Wraps its unevaluated argument in a Par and marks it for concurrent evaluation
   */
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  /**
   * Fully evaluates a given Par, spawning parallel computations as requested by fork and extracting the resulting value
   */
  def run[A](parA: Par[A]): A


  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  /**
   * Combine N parallel computations
   */
  def parMap[A, B](as: List[A])(f: A => B): Par[List[B]] =
    as.foldLeft(unit(scala.List[B]()))((acc, a) => {
      map2(acc, unit(f(a)))((bs, b) => b::bs)
    })

  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldLeft(unit(scala.List[A]()))((acc, parA) => {
      map2(acc, parA)((as, a) => a::as)
    })


}

