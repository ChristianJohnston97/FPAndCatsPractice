package algorithms

import scala.annotation.tailrec

object Fibonaci {

  /**
   * tail call is a subroutine call performed as the final action of a procedure
   * compiler can't turn it into iteration/loop
   *
   */
  def getFibonacci(index: Int): Int = {
    @tailrec
    def getTailRec(index: Int, prev: Int, current: Int): Int = {
      if (index == 0) {
        current
      } else {
        getTailRec(index - 1, prev + current, prev)
      }
    }
    getTailRec(index, 1, 0)
  }
}
