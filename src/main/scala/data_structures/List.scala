package data_structures

import scala.annotation.tailrec
import scala.util.Random

/**
 * Single Linked List
 */
sealed trait MyList[+A] {
  def head: A
  def tail: MyList[A]
  def isEmpty: Boolean
  def headOption: Option[A]
  def ::[S >: A](element: S): MyList[S] // technically due to type variance (S is a supertype of A)
  def get(index: Int): A // get element at index 'index'
  def length: Int
  def reverse: MyList[A]
  def ++[S >: A](list: MyList[S]): MyList[S]
  def removeKthElement(k: Int): MyList[A]

  def map[B](f: A => B): MyList[B]
  def flatMap[B](f: A => MyList[B]): MyList[B]
  def filter(f: A => Boolean): MyList[A]

  def runLength: MyList[(A, Int)]
  def duplicateElements(k: Int): MyList[A]
  def rotate(k: Int): MyList[A]

  def insertionSort[S >:A](ordering: Ordering[S]): MyList[S]
  def mergeSort[S >:A](ordering: Ordering[S]): MyList[S]

  /**
   * Return k random elements from the list
   * k can be bigger than length of list
   */
  def randomSample(k: Int): MyList[A]
}

case object MyListNil extends MyList[Nothing] {
  override def head: Nothing = throw new NoSuchElementException // throwing exceptions is a side effect
  override def tail: MyList[Nothing] = throw new NoSuchElementException
  override def isEmpty: Boolean = true
  override def headOption: Option[Nothing] = None // more idiomatic
  override def toString: String = "[]"
  override def ::[S >: Nothing](element: S): MyList[S] = new ::[S](element, this)
  override def get(index: Int): Nothing = throw new NoSuchElementException
  override def length: Int = 0
  override def reverse: MyList[Nothing] = MyListNil
  override def ++[S >: Nothing](list: MyList[S]): MyList[S] = list
  override def removeKthElement(k: Int): MyList[Nothing] = throw new NoSuchElementException
  override def map[B](f: Nothing => B): MyList[B] = MyListNil
  override def flatMap[B](f: Nothing => MyList[B]): MyList[B] = MyListNil
  override def filter(f: Nothing => Boolean): MyList[Nothing] = MyListNil
  override def runLength: MyList[(Nothing, Int)] = MyListNil
  override def duplicateElements(k: Int): MyList[Nothing] = MyListNil
  override def rotate(k: Int): MyList[Nothing] = MyListNil
  override def randomSample(k: Int): MyList[Nothing] = MyListNil
  override def insertionSort[S >: Nothing](ordering: Ordering[S]): MyList[S] = MyListNil
  override def mergeSort[S >: Nothing](ordering: Ordering[S]): MyList[S] = MyListNil
}

// Cons operator
case class ::[+A](head: A, tail: MyList[A]) extends MyList[A] {
  override def isEmpty: Boolean = false
  override def headOption: Option[A] = Some(head)
  override def toString: String = {

    @tailrec
    def toStringTailRec(remaining: MyList[A], acc: String): String =
      remaining match {
        case MyListNil => acc // empty list, return the acc
        case ::(head, MyListNil) => s"$acc$head" // 1 element left
        case ::(head, tail) => toStringTailRec(tail, s"$acc$head, ") // recursive call is last call
      }
    toStringTailRec(this, "")
  }

  override def ::[S >: A](element: S): MyList[S] = new ::(element, this)

  // complexity O(N) time
  override def get(index: Int): A = {

    @tailrec
    def getTailRec(myList: MyList[A], counter: Int): A = myList match {
      case MyListNil => throw new NoSuchElementException
      case ::(head, tail) =>
        if(counter == index) head
        else getTailRec(tail, counter +1)
    }

    if(index < 0) throw new NoSuchElementException
    getTailRec(this, 0)
  }

  // complexity O(N) time
  override def length: Int = {

    @tailrec
    def getLengthTailRec(remaining: MyList[A], length: Int): Int = {
      remaining match {
        case MyListNil => length
        case ::(_, tail) => getLengthTailRec(tail, length +1)
      }
    }
    getLengthTailRec(this, 0)
  }

  override def reverse: MyList[A] = {

    @tailrec
    def reverseTailRec(listRemaining: MyList[A], accList: MyList[A]): MyList[A] = {
      listRemaining match {
        case MyListNil => accList
        case ::(head, tail) => reverseTailRec(tail, head :: accList)
      }
    }
    reverseTailRec(this, MyListNil)
  }

  // O(M + N)
  override def ++[S >: A](list: MyList[S]): MyList[S] = {

    def concatTailRec(list: MyList[S], acc: MyList[S]): MyList[S] = {
      list match {
        case MyListNil => acc
        case ::(head, tail) => concatTailRec(tail, head :: acc)
      }
    }

    concatTailRec(list, this.reverse).reverse
  }

  override def removeKthElement(k: Int): MyList[A] = {

    @tailrec
    def removeTailRec(previous: MyList[A], remaining: MyList[A], index: Int): MyList[A] = {
      remaining match {
        case MyListNil => throw new NoSuchElementException
        case ::(_, MyListNil) => previous.reverse
        case ::(head, tail) =>
          if(index == k) previous.reverse ++ tail
          else removeTailRec(head::previous, tail, index + 1)
      }
    }
    removeTailRec(MyListNil, this, 0)
  }

  // O(n)
  override def map[B](f: A => B): MyList[B] = {

    @tailrec
    def mapTailRec(remaining: MyList[A], acc: MyList[B]): MyList[B] = {
      remaining match {
        case MyListNil => acc.reverse
        case ::(head, MyListNil) => f(head) :: acc
        case ::(head, tail) => mapTailRec(tail, f(head) :: acc)
      }
    }
    mapTailRec(this, MyListNil).reverse
  }

  override def flatMap[B](f: A => MyList[B]): MyList[B] = {

    @tailrec
    def flatMapTailRec(remaining: MyList[A], acc: MyList[B]): MyList[B] = {
      remaining match {
        case MyListNil => acc.reverse
        case ::(head, tail) => flatMapTailRec(tail, f(head).reverse ++ acc)
      }
    }

    /**
     * [1, 2, 3].flatmap(x => [x, 2 * x]) = concatenateAll([6,4], [4,2], [2,1]. [])
     */
    @tailrec
    def flatMapBetterTailRec(remaining: MyList[A], acc: MyList[MyList[B]]): MyList[B] =
      remaining match {
        case MyListNil => concatenateAll(acc, MyListNil, MyListNil)
        case ::(head, tail) => flatMapBetterTailRec(tail, f(head).reverse :: acc)
      }

    @tailrec
    def concatenateAll(elements: MyList[MyList[B]], current: MyList[B], acc: MyList[B]): MyList[B] =
      current match {
        case MyListNil => elements match {
          case MyListNil => acc
          case ::(head, tail) => concatenateAll(tail, head, acc)
        }
        case ::(head, tail) =>concatenateAll(elements, tail, head :: acc)
      }

    flatMapTailRec(this, MyListNil)
    flatMapBetterTailRec(this, MyListNil) // much faster
  }

  // O(n)
  override def filter(f: A => Boolean): MyList[A] = {

    @tailrec
    def filterTailRec(remaining: MyList[A], acc: MyList[A]): MyList[A] = {
      remaining match {
        case MyListNil => acc.reverse
        case ::(head, tail) =>
          if(f(head))
            filterTailRec(tail, head :: acc)
          else filterTailRec(tail, acc)
      }
    }
    filterTailRec(this, MyListNil)
  }

  // O(N)
  override def runLength: MyList[(A, Int)] = {
    @tailrec
    def runLengthTailRec(remaining: MyList[A], currentTuple: (A, Int), acc: MyList[(A, Int)]): MyList[(A, Int)] =
      remaining match {
        case MyListNil =>
          if(currentTuple._2 == 0) acc
          else currentTuple :: acc
        case ::(head, tail) =>
          if(head == currentTuple._1)
            runLengthTailRec(tail, (head, currentTuple._2 + 1), acc)
          else runLengthTailRec(tail, (tail.head, 1), currentTuple :: acc)
      }
    runLengthTailRec(this, (this.head, 1), MyListNil)
  }

  override def duplicateElements(k: Int): MyList[A] = {

    @tailrec
    def duplicateElementsTailRec(remaining: MyList[A], counter: Int, acc: MyList[A]): MyList[A] = {
      remaining match {
        case MyListNil => acc.reverse
        case ::(head, tail) =>
          if(counter == k)  duplicateElementsTailRec(tail, 0, acc)
          else duplicateElementsTailRec(head :: tail, counter+1, head::acc)
      }
    }
    duplicateElementsTailRec(this, 0, MyListNil)
  }

  // O(max(N, k))
  override def rotate(k: Int): MyList[A] = {
    @tailrec
    def rotateTailRec(remaining: MyList[A], counter: Int, acc: MyList[A]): MyList[A] = remaining match {
      case MyListNil =>
        if(counter == k) this
        else rotateTailRec(this, counter, MyListNil) // pass original list back in
      case ::(head, tail) =>
        if(counter == k)
          tail ++ acc.reverse
        else rotateTailRec(tail, counter+1, head :: acc)
    }
    rotateTailRec(this, 0, MyListNil)
  }


  // O(NK)
  // get is O(n), k times
  override def randomSample(k: Int): MyList[A] = {

    val maxIndex = this.length
    val randomNum = new Random(System.currentTimeMillis())
    @tailrec
    def randomSampleTailRec(counter: Int, acc: MyList[A]): MyList[A] = {
      if(counter == k) acc
      else {
        val index = randomNum.nextInt(maxIndex) // some random index in original list
        val randomA = this.get(index)
        randomSampleTailRec(counter +1, randomA :: acc)
      }
    }
    randomSampleTailRec(0, MyListNil)
  }

  /**
   * Insertion Sort
   * Take each element and insert it in a sorted fashion
   * Complexity: O(nn)
   */
  override def insertionSort[S >: A](ordering: Ordering[S]): MyList[S] = {

    /*
      [3,1,2,4,5].sorted = sortedTailRec([3,1,2,4,5], [])
        = sortedTailRec([1,2,4,5], [3])
        = sortedTailRec([2,4,5], [1,3])
        = sortedTailRec([4,5], [1,2,3])
        = sortedTailRec([5], [1,2,3,4])
        = sortedTailRec([], [1,2,3,4,5])
        = [1,2,3,4,5]
        Complexity: O(n)
     */
    @tailrec
    def sortedTailRec(remaining: MyList[S], acc: MyList[S]): MyList[S] = {
      remaining match {
        case MyListNil => acc
        case ::(head, tail) => sortedTailRec(tail, insertSorted(head, MyListNil, acc))
      }
    }

    /*
      insertSorted(4, [], [1,2,5])
      insertSorted(4, [1], [2,5])
      insertSorted(4, [2,1], [5])
      insertSorted(4, [4,2,1], [5])
      = [4,2,1].reverse :: [5]
      = [1,2,4,5]
      Complexity: O(n)
     */
    @tailrec
    def insertSorted(s: S, left: MyList[S], right: MyList[S]): MyList[S] =
      right match {
        case MyListNil => left.reverse ++ (s::MyListNil)
        case ::(head, tail) =>
          if(ordering.lteq(s, head)) // s is bigger than head
            left.reverse ++ (head :: right)
          else insertSorted(s, head :: left, tail)
      }
    sortedTailRec(this, MyListNil)
  }

  /**
   * Split list in half, sort each half recursively
   * then merge halves in sorted fashion
   */
  override def mergeSort[S >: A](ordering: Ordering[S]): MyList[S] = {

    /*
      [3,1,2,5,4] => [[3], [1], [2], [5], [4]]
      mergeSortTailRec([[3], [1], [2], [5], [4]], [])
      = mergeSortTailRec([[2], [5], [4]], [[1,3]])
      = mergeSortTailRec([[4]], [[2,5], [1,3]])
      = mergeSortTailRec([]. [[4], [2,5], [1,3]])
      = mergeSortTailRec([[4], [2,5], [1,3]], [])
      = mergeSortTailRec([[1,3]], [[2,4,5]])
      = mergeSortTailRec([], [[1,3], [2,4,5]])
      = mergeSortTailRec([[1,3], [2,4,5]], [])
      = mergeSortTailRec([], [[1,2,3,4,5]])
      = [1,2,3,4,5]
     */
    @tailrec
    def mergeSortTailRec(smallList: MyList[MyList[S]], bigList: MyList[MyList[S]]): MyList[S] =
      smallList match {
        case MyListNil =>
          bigList match {
            case MyListNil => MyListNil
            case ::(head, tail) => tail match {
              case MyListNil => bigList.head
              case ::(head, tail) => mergeSortTailRec(bigList, MyListNil)
            }
          }
        case ::(head, tail) => tail match {
          case MyListNil => mergeSortTailRec(head :: bigList, MyListNil)
          case ::(head, tail) =>
            val first = head
            val second = tail.head
            val merged = merge(first, second, MyListNil)
            mergeSortTailRec(tail.tail, merged :: bigList)
        }
      }

    /*
    merge([1,2,3], [4,5,6])
      = [1,2,3,4,5,6]
     */
    def merge(sortedList1: MyList[S], sortedList2: MyList[S], acc: MyList[S]): MyList[S] = {
      if(sortedList1.isEmpty) acc.reverse ++ sortedList2
      else if (sortedList2.isEmpty) acc.reverse ++ sortedList1
      else if (ordering.lteq(sortedList1.head, sortedList2.head))
        merge(sortedList1.tail, sortedList2, sortedList1.head :: acc)
      else merge(sortedList1, sortedList2.tail, sortedList2.head :: acc)
    }

    mergeSortTailRec(this.map(x => x :: MyListNil), MyListNil)
  }
}

// companion object
object MyList {

  // e.g. passing in a range
  def from[A](iterable: Iterable[A]): MyList[A] = {
    @tailrec
    def convertToListTailRec(remaining: Iterable[A], acc: MyList[A]): MyList[A] = {
      if(remaining.isEmpty) acc.reverse
      else convertToListTailRec(remaining.tail, remaining.head :: acc)
    }
    convertToListTailRec(iterable, MyListNil).reverse
  }
}



object ListProblems extends App {
  val newList: MyList[Int] = 1 :: 2 :: 3 :: 4 :: MyListNil // right associative
  val newList2: MyList[Int] = 5 :: 6 :: 7 :: 8 :: MyListNil

  println(newList.get(2))
  println(newList.length)
  println(newList.reverse)
  println(newList ++ newList2)
  println(newList.removeKthElement(2))
  println(newList.map(_+2))
  println(newList.flatMap( x => x :: (x*2) :: MyListNil))
  println(newList.duplicateElements(2))
}
