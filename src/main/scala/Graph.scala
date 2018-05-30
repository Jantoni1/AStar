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

class Graph[T, V: Ordering : Numeric, N <: Heuristics[T,V]](val edges: Set[Edge[N,V]], val vertices : Set[Node[N]]) {

  def addVertex(node: Node[N]) = new Graph[T, V, N](edges, vertices + node)

  def removeVertex(node: Node[N]) = new Graph[T, V, N](edges.filter(_.contains(node)), vertices - node)

  def addEdge(edge: Edge[N, V]) = new Graph[T, V, N](edges + edge +
    Edge(edge.node2, edge.node1, edge.weight), vertices + edge.node1 + edge.node2)

  def removeEdge(edge: Edge[N, V]) = new Graph[T, V, N](edges.filter(_ != edge), vertices)

  def getEdge(source: Node[N], destination: Node[N]): Edge[N, V] = edges.filter(edge =>
    edge.node1 == source && edge.node2 == destination).head

  lazy val edgeMap: Map[N, Set[Node[N]]] = edges.groupBy(_.node1.value).mapValues(s => s.map(_.node2))

  private def sum(list: List[V])(implicit n: Numeric[V]): V = list.reduce((x: V, y: V) => n.plus(x, y))




  def routeBetween(source: N, destination: N): (V, List[Node[N]]) = {

    case class Route(alreadyVisited: List[Node[N]]) {
      def cost(): V = {
        val visitedEdges = alreadyVisited.zip(alreadyVisited.tail)
        sum(alreadyVisited.last.value.heuristicValue(destination) ::
          visitedEdges.map { case (n1, n2) => getEdge(n1, n2).weight })
      }
    }

    def beginJourney(source: N): Route = {
      discoverNewRoutes(List(Route(List(Node[N](source)))))
    }

    @tailrec
    def discoverNewRoutes(allRoutes: List[Route]): Route = {
      val sortedByCost = allRoutes.sortBy(_.cost())
      val cheapestRoute = sortedByCost.headOption.getOrElse(sys.error("An error occurred."))
      val latestNode = cheapestRoute.alreadyVisited.last.value
      latestNode match {
        case `destination` => cheapestRoute
        case cccMatched =>
          val newRoutes = for (neighbourNode <- edgeMap(cccMatched).toList.filter(!cheapestRoute.alreadyVisited.contains(_)))
            yield Route(cheapestRoute.alreadyVisited :+ neighbourNode)
          discoverNewRoutes(newRoutes ++ sortedByCost.tail)
      }
    }

    val route = beginJourney(source)
    (route.cost(), route.alreadyVisited)
  }
}


//private def oddElements(arr: List[V]): List[V] = arr.indices.collect { case i if i % 2 == 0 => arr(i) }.toList


//  def aStar(sourceNode : Node[N], destination : Node[N], initialCost : V)(implicit n: Numeric[V]) : List[Node[N]]  = {
//
//    val initialHeuristics: V = sourceNode.value.heuristicValue(destination.value)
//
//    val edgeSet : List[V] = oddElements(edges.toList.unzip3[Node[N], Node[N], V](edge =>
//      (edge.node1, edge.node2, edge.weight))._3.sorted)
//    edgeSet.foreach(println)
//    val shortestPaths = edges.toList.sortBy(edge =>
//      (edge.node1.value.heuristicValue(edge.node2.value), edge.node1, edge.node2)).take(2 * vertices.size)
//    @tailrec
//    def iteration(source: Node[N], cost: V, heuristics: V, path: List[Node[N]]): List[Node[N]] = source match {
//      case src if src == destination => path ++ List(src)
//      case _ =>
//        val newNode = edgeMap.apply(source.value).minBy(node =>
//          sum(List(cost, getEdge(source, node).weight, node.value.heuristicValue(destination.value))))
//    iteration(
//          newNode,
//          sum(List(cost, getEdge(source, newNode).weight)),
//          newNode.value.heuristicValue(destination.value),
//          path ++ List(source))
//    }
//
//    iteration(sourceNode, initialCost, initialHeuristics, Nil)
//  }