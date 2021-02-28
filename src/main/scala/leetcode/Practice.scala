package leetcode

import scala.collection.mutable

object Practice {

  def reverseCharArray(array: List[Char]): List[Char] = {
    array match {
      case Nil => array
      case ::(head, next) => reverseCharArray(next) :+ head
    }
  }

  def reverseCharArrayInPlace(array: Array[Char]): List[Char] = {
    var end = array.length
    for(i <- 0 until end){
      array(i) = array(end)
      end -= 1
    }
    array.toList
  }

  // O(n*2)
  def twoSum(array: List[Int], target: Int): (Int, Int) = {
    for(i <- array.indices){
      for(j <- array.indices){
        if(array(i)+array(j) == target){
          return (i, j)
        }
      }
    }
    (0, 0)
  }

  def swapAdjacentNodes(headNode: Node): Node = {

    def swap(head: Option[Node]): Option[Node] = {
      head match {
        case Some(node) =>
          node.next match {
          case Some(node) =>
            node.next = head
            swap(head.get.next.get.next)
          case None => head
        }
        case None => None
      }
    }
    swap(Some(headNode)).get
  }

  /**
   * Better solution
   * We take two pointers, one representing the first element and other representing the last element of the array,
   * and then we add the values kept at both the pointers. If their sum is smaller than X then we shift the left
   * pointer to right or if their sum is greater than X then we shift the right pointer to left,
   * in order to get closer to the sum. We keep moving the pointers until we get the sum as X.
   */
  def twoSumBetter(array: Array[Int], target: Int): Int = { // represents first pointer
    var i = 0
    var j = array.length - 1
    while (i < j)
      if (array(i) + array(j) == target)
        return 1
      else
        if (array(i) + array(j) < target)
          i += 1
        else j -= 1
    0
  }




  case class Node(data: Int, var next: Option[Node])
  /**
   * Create 2 runners, one k moves in front of the other
   */
  def q19(headNode: Node, k: Int): Node = {

    var runner1 = headNode
    var runner2 = headNode
    var nodeBefore = headNode

   for(_ <- 0 to k) {
     if (runner1.next.isDefined) {
       runner1 = runner1.next.get
     }
   }

    if(runner1.next.isDefined){
      runner1 = runner1.next.get
      runner2 = runner2.next.get
    }

    while(runner1.next.isDefined){
      runner1 = runner1.next.get
      runner2 = runner2.next.get
      nodeBefore = nodeBefore.next.get
    }
    nodeBefore.copy(next = runner2.next)
   }


  def q20(string: String): Boolean = {
    val stack = mutable.Stack[Char]()
    val charArray = string.toCharArray
    val openBrackets = List("(", "[", "{")
    val closedBrackets = List(")", "]", "}")
    for(char <- charArray) yield {
      if(openBrackets.contains(char.toString)){
        stack.push(char)
      }
      if(closedBrackets.contains(char.toString)){
        stack.pop
      }
    }
    if(stack.isEmpty) true else false
  }




}
