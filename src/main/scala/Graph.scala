case class Node[T](i: T)

class Graph[T, V](val edges : Set[(Node[T], Node[T], V)], val vertices : Set[Node[T]]) {

  def addVertex(node: Node[T]) = new Graph[T, V](edges, vertices + node)

  def removeVertex(node: Node[T]) = new Graph[T,V](edges.filter(p => p._1 != node && p._2 != node), vertices - node)

  def addEdge(edge: (Node[T], Node[T], V)) = new Graph[T, V](edges ++ Set(edge, (edge._2, edge._1, edge._3)), vertices + edge._1 + edge._2)

  lazy val edgeMap: Map[Node[T], Set[Node[T]]] = edges.groupBy(_._1).mapValues(s => s.map(_._2))

}