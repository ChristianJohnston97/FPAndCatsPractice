package algorithms

import scala.annotation.tailrec

object InsertionSort {

  /**
   * Insertion sort not tail recursive
   */
  def insertionSort(list: List[Int]): List[Int] = {
    def insertSorted(element: Int, sorted: List[Int]): List[Int] =
      if (sorted.isEmpty || element < sorted.head) element :: sorted
      else sorted.head :: insertSorted(element, sorted.tail)

    if (list.isEmpty || list.tail.isEmpty) list
    else insertSorted(list.head, insertionSort(list.tail))
  }

  /**
   * Insertion sort tail recursive
   * In acc we’ll store all the numbers smaller than element.
   * At the moment when element <= sortedList.head, all the smaller numbers of the result are in accumulator (in reverse order)
   * and all the bigger numbers are in sortedList.
   */
  def insertionSortTailRecursive(list: List[Int]): List[Int] = {

    @tailrec
    def insertSorted(element: Int, sorted: List[Int], acc: List[Int]): List[Int] =
      if (sorted.isEmpty || element <= sorted.head) acc.reverse ++ (element :: sorted)
      else insertSorted(element, sorted.tail, sorted.head :: acc)

    @tailrec
    def sortTailrec(list: List[Int], accumulator: List[Int]): List[Int] =
      if (list.isEmpty) accumulator
      else sortTailrec(list.tail, insertSorted(list.head, accumulator, Nil))

    sortTailrec(list, Nil)
  }
}