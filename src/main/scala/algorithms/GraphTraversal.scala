package algorithms

object GraphTraversal {

  type Vertex=Int
  type Graph=Map[Vertex, List[Vertex]]

  def DFS(start: Vertex, graph: Graph): List[Vertex] = {

    def DFSInner(v: Vertex, visited: List[Vertex]): List[Vertex] = {
      if (visited.contains(v))
        visited
      else {
        val neighbours = graph(v) filterNot visited.contains
        neighbours.foldLeft(v :: visited)((b,a) => DFSInner(a,b))
      }
    }
    DFSInner(start, List()).reverse
  }

  def BFS(start: Vertex, graph: Graph): List[List[Vertex]] = {

    def BFSInner(elems: List[Vertex], visited: List[List[Vertex]]): List[List[Vertex]] = {
      val newNeighbors = elems.flatMap(graph(_)).filterNot(visited.flatten.contains).distinct
      if (newNeighbors.isEmpty)
        visited
      else
        BFSInner(newNeighbors, newNeighbors :: visited)
    }
    BFSInner(List(start), List(List(start))).reverse
  }

}
