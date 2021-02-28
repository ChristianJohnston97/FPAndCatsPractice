package algorithms

import scala.collection.mutable

case class Node(var visited: Boolean, var marked: Boolean, adjacent: List[Node])

object BFS {

  def bfsSearch(startNode: Node): Unit = {
    val queue: mutable.Queue[Node] = mutable.Queue()
    queue.enqueue(startNode)
    startNode.visited = true
    startNode.marked = true
    while(queue.nonEmpty){
      val frontNode = queue.dequeue
      frontNode.visited = true
      for(node <- frontNode.adjacent){
        if(!node.visited){
          node.marked = true
          queue.enqueue(node)
        }
      }
    }
  }
}
