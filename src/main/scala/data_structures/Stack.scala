package data_structures

case class Node(data: Int, next: Option[Node])

class Stack(topNode: Option[Node]){

  def pop: Option[Node] = {
    topNode match {
      case Some(node) => node.next
      case None => None
    }
  }

  def push(data: Int): Node = {
    Node(data, topNode)
  }

  def peek: Option[Node] = {
    topNode match {
      case Some(node) => Some(node)
      case None => None
    }
  }

  def isEmpty: Boolean = topNode match {
    case Some(_) => false
    case None => true
  }
}

