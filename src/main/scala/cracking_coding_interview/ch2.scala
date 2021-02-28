package cracking_coding_interview

object ch2 extends App {

  /**
   * LinkedList node
   */
  case class Node(var value: Int, var next: Option[Node])

  /**
   * Returns the head of the linked list
   */
  def deleteNode(startingNode: Node, d: Int): Option[Node] = {
    var node = startingNode
    if(node.value == d) {
      return node.next // return new moved head
    }

    while(node.next.isDefined){
      if(node.next.get.value == d){
        node.next = node.next.get.next
      }
      node = node.next.get
    }
    Some(startingNode)
  }

  /**
   * Q 2.1
   */
  def removeDupes(node: Node): Unit = {
    var frequencyMap: Map[Int, Int] = Map(node.value -> 1)
    var previous: Node = null
    while(node.next.isDefined){
      if(frequencyMap.contains(node.next.get.value)){
        previous.next = node.next
      }
      else {
        frequencyMap += (node.next.get.value -> 1)
        previous = node
      }
      node.next
    }
  }

  /**
   * Q 2.2
   * Have 2 pointers. Move the first one k ahead at the beginning
   * Then move them both until the one ahead reaches the end
   * the trailing one will then be at k'th last element
   */
  def kthLastElement(startNode: Node, k: Int): Node = {
    var node = startNode
    for(i <- 0 until k){
      if(node.next.isDefined){
        node = node.next.get
      }
    }
    var trailingNode = startNode
    while(node.next.isDefined){
      node = node.next.get
      trailingNode = trailingNode.next.get
    }
    trailingNode
  }

  /**
   * Q2.3
   * Copy the data from the next node into the middle node
   * and then delete the next node
   */
  def deleteMiddleNode(middleNode: Node): Unit = {
    val nextNode = middleNode.next
    if(nextNode.isEmpty){
      println("Error!")
    }
    else {
      middleNode.value = nextNode.get.value
      middleNode.next = nextNode.get.next
    }
  }

  /**
   * Q 2.4
   */
  def partition(startingNode: Node, value: Int): Node = {
    val head = startingNode
    val tail = startingNode
    var node = startingNode
    while(node.next.isDefined){
      if(node.next.get.value >= value){
        head.next = node.next
      }
      else {
        node.next = Some(tail)
      }
      node = node.next.get
    }
    head
  }

  def palindrome(startingNode: Node): Boolean = {
    var mutableString = startingNode.value.toString
    while(startingNode.next.isDefined){
      val data = startingNode.next.get.value
      mutableString += data.toString
    }
    if(mutableString.reverse == mutableString) true else false
  }

  /**
   * Q 2.8
   */
  def loop(startingNode: Node): Node = ???
}
