package tutorial

import algorithms.Node

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object ScalaTut {

  object map {

    var map: Map[String, Int] = Map("string1" -> 1, "string2" -> 2)
    val mapValueByKey: Int = map("string1")
    map += ("string3" -> 3)
  }

  object Queue {
    var queue: mutable.Queue[Node] = mutable.Queue()

  }

  object MutableList {
    var array: ListBuffer[Int] = ListBuffer[Int]()
    array += 2
    array.toList
  }

}
