import scala.annotation.tailrec

case class Node[T](value: T)

case class Edge[T, V](node1: Node[T], node2: Node[T], weight: V) {
  override def equals(obj: scala.Any): Boolean = obj match {
    case edge: Edge[T,V] =>
      ((node1 == edge.node1 && node2 == edge.node2) ||
        (node1 == edge.node2 && node2 == edge.node1)) && weight == edge.weight
    case _ => false
  }

  def contains(node: Node[T]): Boolean = node1 == node || node2 == node
}

class Graph[T, V, N <: Heuristics[T,V]](val edges: Set[Edge[N,V]], val vertices : Set[Node[N]]) {

  def addVertex(node: Node[N]) = new Graph[T, V, N](edges, vertices + node)

  def removeVertex(node: Node[N]) = new Graph[T, V, N](edges.filter(_.contains(node)), vertices - node)

  def addEdge(edge: Edge[N, V]) = new Graph[T, V, N](edges + edge +
    Edge(edge.node2, edge.node1, edge.weight), vertices + edge.node1 + edge.node2)

  def removeEdge(edge: Edge[N, V]) = new Graph[T, V, N](edges.filter(_ != edge), vertices)

  def getEdge(source : Node[N], destination : Node[N]) : Edge[N,V] = edges.filter(edge =>
    edge.node1 == source && edge.node2== destination).head

  lazy val edgeMap: Map[N, Set[Node[N]]] = edges.groupBy(_.node1.value).mapValues(s => s.map(_.node2))

  private def sum(x: V, y: V)(implicit n: Numeric[V]): V = n.plus(x, y)

  def aStar(sourceNode : Node[N], destination : Node[N], initialCost : V)(implicit n: Numeric[V]) : List[Node[N]]  = {
    val initialHeuristics: V = sourceNode.value.heuristicValue(destination.value)
    @tailrec
    def iteration(source: Node[N], cost: V, heuristics: V, path: List[Node[N]]): List[Node[N]] = source match {
      case src if src == destination => path ++ List(src)
      case _ =>
        val newNode = edgeMap.apply(source.value).minBy(node =>
          sum(sum(cost, getEdge(source, node).weight), node.value.heuristicValue(destination.value)))
    iteration(
          newNode,
          sum(cost, getEdge(source, newNode).weight),
          newNode.value.heuristicValue(destination.value),
          path ++ List(source))
    }
    iteration(sourceNode, initialCost, initialHeuristics, Nil)
  }
}
