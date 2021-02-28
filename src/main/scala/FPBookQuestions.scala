import scala.::

object FPBookQuestions {

  // 2.3
  def curry[A, B, C](f: (A, B) => C): A => B => C = {
    a => b => f(a, b)
  }

  // 2.4
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a, b) => f(a)(b)
  }

  // 2.5
  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    a => f(g(a))
  }

  // 3.2
  def tail[A](fa: List[A]): List[A] = {
    fa match {
      case _::tail => tail
      case Nil => List() // or throw error
    }
  }
  // 3.3
  def setHead[A](fa: List[A], b: A): List[A] = {
    fa match {
      case head::tail => b::tail
      case Nil => List() // or throw error
    }
  }

  // 3.4
  def drop[A](fa: List[A], n: Int): List[A] = {
    if(n > fa.length)
      List() // or throw error
    fa match {
      case head::tail => drop(tail, n-1)
      case Nil => List() // or throw error
    }
  }

  // 3.5
  def dropWhile[A](fa: List[A], f: A => Boolean): List[A] = {
    fa match {
      case head::tail =>
        if(f(head)) {
          dropWhile(tail, f)
        }
        else {
          tail
        }
      case Nil => List() // or throw error
    }
  }

  // 3.6
  def init[A](fa: List[A]): List[A] = {
    fa match {
      case head::tail => head::init(tail)
      case _::Nil => Nil
      case Nil => fa
    }
  }

  //3.10
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case head::tail => foldLeft(tail, f(z, head))(f)
      case Nil => z
    }
  }

  //3.11
  def sum(list: List[Int]): Int = {
    list.foldLeft(0)(_ + _)
  }

  def length[A](list: List[A]): Int =
    list.foldLeft(0)((sum, _) => sum + 1)


  // 4.3
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a.flatMap(aVal => b.map(bVal => f(aVal, bVal)))
  }

  // 4.4
  def sequence[A](as: List[Option[A]]): Option[List[A]] = {
    as match {
      case head::tail  => head.flatMap(h => sequence(tail) map (h :: _))
      case Nil => Some(Nil)
    }
  }

  // 4.5
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a match {
      case head::tail => map2(f(head), traverse(tail)(f))(_ :: _)
      case Nil => Some(Nil)
    }
  }

  // 5.5
  def takeWhileFold[A](list: List[A], f: A => Boolean): List[A] = {
    list.foldLeft(List[A]())((a,b) => {
      if(f(b)) b :: a
      else List()
    })
  }

  // 5.6
  def headOptionFold[A](list: List[A]): Option[A] =
    list.foldRight(None: Option[A])((b, _) => Some(b))

  // 5.7
  def mapFold[A, B](list: List[A], f: A => B): List[B] =
    list.foldLeft(List[B]())((b, a) => f(a) :: b)

  def append[A](list: List[A], a: A): List[A] =
    list.foldLeft(List(a))((b, _) => a :: b)


  /**
   * Make state updates explicit
   * Separate the concern of computing the state from the communication of the new state to rest of program
   */
  trait State {
    def next[A]: (A, State)
  }

  type StateAction[A] = State => (A, State)

  object StateAction {
    def unit[A](a: A): StateAction[A] = ???
  }


  def map[A, B](s: StateAction[A])(f: A => B): StateAction[B] = {
    state => {
      val (a, newState) = s(state)
      (f(a), newState)
    }
  }

  def map2[A, B, C](stateA: StateAction[A], stateB: StateAction[B])(f: (A, B) => C): StateAction[C] = {
    state => {
      val (a, newState) = stateA(state)
      val (b, newState2) = stateB(newState)
      (f(a, b), newState2)
    }
  }

  def sequence[A](list: List[StateAction[A]]): StateAction[List[A]] = {
    list.foldLeft(StateAction.unit(List[A]()))((acc, a) => map(a, acc))
  }

  def flatMap[A, B](s: StateAction[A])(f: A => StateAction[B]): StateAction[B] = {
    state => {
      val (a, newState) = s(state)
      f(a)(newState)
    }
  }


}
