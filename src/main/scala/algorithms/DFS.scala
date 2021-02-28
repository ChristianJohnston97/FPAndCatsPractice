package algorithms

case class DFSNode(var visited: Boolean, children: List[DFSNode])

object DFS {

  def visitDFSNode(DFSNode: DFSNode): Unit = println(s"visited this DFSNode ${DFSNode.toString}")

  /**
   * 1. Start from root DFSNode
   * 2. Mark current DFSNode as visited
   * 3. visit current DFSNode
   * 4. Traverse unvisited adjacent vertices
   */
  def DFS(startDFSNode: DFSNode): Unit = {
    def innerDFS(DFSNode: DFSNode): Unit = {
      visitDFSNode(DFSNode)
      DFSNode.visited = true
      for (DFSNode <- DFSNode.children) {
        if (!DFSNode.visited) {
          innerDFS(DFSNode)
        }
      }
    }
    innerDFS(startDFSNode)
  }
}

