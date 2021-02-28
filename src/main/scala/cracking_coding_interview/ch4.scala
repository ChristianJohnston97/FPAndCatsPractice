package cracking_coding_interview

import scala.collection.mutable.ListBuffer

object ch4 {

  case class Node(var visited: Boolean, adjacent: List[Node])

  /**
   * Q 4.1
   * Is there a route between 2 nodes. Can do any search algo such as DFS or BFS
   */
  def routeBetweenNodes(start: Node, end: Node): Boolean = {

    def depthFirstSearch(startNode: Node, endNode: Node): Boolean = {
      if(start == endNode){
        return true
      }
      startNode.visited = true
      for(node <- startNode.adjacent){
          node.visited = true
          depthFirstSearch(node, endNode)
      }
      false
    }
    depthFirstSearch(start, end)
  }

  /**
   * Q4.2
   * Given a sorted array with unique integer elements
   * Create a binary search tree with minimal height
   */
  case class TreeNode(data: Int, var left: Option[TreeNode], var right: Option[TreeNode])
  def minimalTree(list: List[Int]): Option[TreeNode] = {

    def createTree(list: List[Int], start: Int, end: Int): Option[TreeNode] = {
      val middle = (start+end)/2
      val root = TreeNode(list(middle), None, None)
      root.left = createTree(list, start, middle)
      root.right = createTree(list, middle, end)
      Some(root)
    }
    createTree(list, 0, list.length)
  }

  /**
   * Q 4.3
   * Binary tree
   * Linked list of all nodes at each depth
   */
  def listOfDepths(node: TreeNode): List[List[Int]] = {

    def createListAtLevel(root: Option[TreeNode], lists: List[List[Int]], level: Int): List[List[Int]] = {
      root match {
        case Some(node) =>
          if (lists.length == level) {
            // list has already been created, add to it
            lists(level) ++ List(node.data)
          }
          else {
            //create list
            lists ++ List(node.data)
          }
          createListAtLevel(node.left, lists, level+1)
          createListAtLevel(node.right, lists, level+1)
        case None => lists
      }
    }
    createListAtLevel(Some(node), List(List[Int]()), 0)
  }

  /**
   * Q4.4
   * Check whether binary tree is balanced
   */
  def checkBalanced(root: TreeNode): Boolean = {

    def getTreeHeight(root: Option[TreeNode], count: Int): Int = {
      root match {
        case Some(node) => Math.max(getTreeHeight(node.left, count+1), getTreeHeight(node.right, count+1))
        case None => count
      }
    }

    def areHeightsSame(root: Option[TreeNode]): Boolean = {
      root match {
        case Some(node) =>
          if(getTreeHeight(node.left, 0) != getTreeHeight(node.right, 0)){
            false
          }
          else {
            areHeightsSame(root.get.left) && areHeightsSame(root.get.right)
          }
        case None => true
      }
    }
    areHeightsSame(Some(root))
  }


  /**
   * Q 4.5
   * Validate whether a binary tree is a binary search tree
   * In-order traversal to array, then check array is in order
   *
   */
  def validateBST(node: TreeNode): Boolean = {
    var array = ListBuffer[Int]()
    var i = 0;
    def copyTreeToArray(node: Option[TreeNode]): Unit = {
      node match {
        case Some(node) =>
          copyTreeToArray(node.left)
          array += node.data
          i+=1
          copyTreeToArray(node.right)
        case None => ()
      }
    }
    copyTreeToArray(Some(node))

    array.toList.reverse == array.toList
  }

  /**
   * Q4.7
   */
  case class NewNode(value: Int, var children: List[NewNode])
  case class NewTree(var list: List[NewNode])
  def builderOrder(list: List[(Int, Int)]): NewTree = {
    val tree: NewTree = NewTree(List())
    for {
    pair <- list
    } yield {
      val firstElement = pair._1
      val secondElement = pair._2
      val newNode = NewNode(firstElement, List(NewNode(secondElement, List())))

    }

  }


}
