package main

import scala.annotation.tailrec

/**
  * Represents a single node (vertex) of a graph
  *
  * @param value value held by a node
  * @tparam T type of value
  */
case class Node[T](value: T)

/**
  * Represents a single edge between two nodes
  *
  * @param node1 start node
  * @param node2 end node
  * @param weight weight of the connection
  * @tparam T type of values held by nodes
  * @tparam V type of weight
  */
case class Edge[T, V](node1: Node[T], node2: Node[T], weight: V) {
  override def equals(obj: scala.Any): Boolean = obj match {
    case edge: Edge[T,V] =>
      ((node1 == edge.node1 && node2 == edge.node2) ||
        (node1 == edge.node2 && node2 == edge.node1)) && weight == edge.weight
    case _ => false
  }

  /**
    * Checks whether edge starts or ends in given node
    *
    * @param node node to check
    * @return
    */
  def contains(node: Node[T]): Boolean = node1 == node || node2 == node
}

/**
  * Represents a graph which is a set of nodes connected by edges
  *
  * @param edges set of edges
  * @param vertices set of nodes
  * @tparam T see [[main.Heuristics]]
  * @tparam V see [[main.Heuristics]]
  * @tparam N type of values held by nodes
  */
class Graph[T, V: Ordering : Numeric, N <: Heuristics[T,V]](val edges: Set[Edge[N,V]], val vertices : Set[Node[N]]) {

  /**
    * Adds new vertex (node) to the graph
    *
    * @param node node to be added
    * @return updated graph
    */
  def addVertex(node: Node[N]) = new Graph[T, V, N](edges, vertices + node)

  /**
    * Removes given vertex (node) from the graph
    *
    * @param node node to be removed
    * @return updated graph
    */
  def removeVertex(node: Node[N]) = new Graph[T, V, N](edges.filter(_.contains(node)), vertices - node)

  /**
    * Adds new edge between two nodes
    *
    * If any of the nodes is not present in the graph, it is automatically added
    *
    * @param edge edge to be added
    * @return updated graph
    */
  def addEdge(edge: Edge[N, V]) = new Graph[T, V, N](edges + edge +
    Edge(edge.node2, edge.node1, edge.weight), vertices + edge.node1 + edge.node2)

  /**
    * Removes edge from the graph
    *
    * @param edge edge to be removed
    * @return updated graph
    */
  def removeEdge(edge: Edge[N, V]) = new Graph[T, V, N](edges.filter(_ != edge), vertices)

  /**
    * Gets edge between two passed nodes
    *
    * @param source source node
    * @param destination destination node
    * @return desired edge
    */
  def getEdge(source: Node[N], destination: Node[N]): Edge[N, V] = edges.filter(edge =>
    edge.node1 == source && edge.node2 == destination).head

  /**
    * For every node returns a set of nodes in direct neighbourhood, where keys are values held by nodes
    */
  lazy val edgeMap: Map[N, Set[Node[N]]] = edges.groupBy(_.node1.value).mapValues(s => s.map(_.node2))

  /**
    * Returns sum of elements passed in list
    *
    * @param list list of elements
    * @param n implicit parameter used to store sum
    * @return sum of elements
    */
  private def sum(list: List[V])(implicit n: Numeric[V]): V = list.reduce((x: V, y: V) => n.plus(x, y))

  /**
    * TODO
    *
    * @param source
    * @param destination
    * @param n
    * @return
    */
  def routeBetween(source: N, destination: N)(): (V, List[Node[N]]) = {

    /**
      * TODO
      *
      * @param alreadyVisited
      */
    case class Route(alreadyVisited: List[Node[N]]) {
      def cost(): V = {
        val visitedEdges = alreadyVisited.zip(alreadyVisited.tail)
        sum(alreadyVisited.last.value.heuristicValue(destination) ::
          visitedEdges.map { case (n1, n2) => getEdge(n1, n2).weight })
      }
    }

    /**
      * TODO
      *
      * @param source
      * @return
      */
    def beginJourney(source: N): Route = {
      discoverNewRoutes(List(Route(List(Node[N](source)))))
    }


    /**
      * TODO
      *
      * @param allRoutes
      * @return
      */
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