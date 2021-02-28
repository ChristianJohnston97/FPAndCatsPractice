package data_structures

case class Node(data: Int, next: Option[Node])

class LinkedList(headNode: Node) {

  /**
   * Access linked list through a reference to the head Node of the linked list
   */
  def appendToTail(data: Int): Node = {
    val node: Node = Node(data, None)
    var currentNode = headNode
    while(currentNode.next.isDefined) {
      currentNode = currentNode.next.get
    }
    Node(currentNode.data, Some(node))
  }

  def deleteNode(data: Int): Node = {
    val currentNode = headNode
    while(currentNode.next.isDefined) {
      if(currentNode.data == data) {
        Node(currentNode.data, currentNode.next.get.next)
      }
      currentNode.next
    }
    currentNode
  }
}
