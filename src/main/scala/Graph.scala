case class Node[T](i: T)

class Graph[T, V](val edgeSet : Set[(Node[T], Node[T], V)], someNodes : Set[Node[T]]) {

  def addVertex(node: Node[T]) = new Graph[T, V](edgeSet, someNodes + node)


}
