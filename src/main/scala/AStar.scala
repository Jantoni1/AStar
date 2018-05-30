import scala.annotation.tailrec

case class AStar[T <: Heuristics](graph: Graph[T, Double]) {

  def routeBetween(source: T, destination: T): (Double, List[Node[T]]) = {

    case class Route(alreadyVisited: List[Node[T]]) {
      def cost(): Double = {
        val weights = for (edge <- alreadyVisited.sliding(2))
          yield edge match {
            case List(e1, e2) => graph.getEdgeWeight(e1, e2)
            case _ => 0
          }
        alreadyVisited.last.i.heuristicValue(destination) + weights.sum
      }
    }

    def beginJourney(source: T): Route = {
      discoverNewRoutes(List(Route(List(Node[T](source)))))
    }

    @tailrec
    def discoverNewRoutes(allRoutes: List[Route]): Route = {
      val sortedByCost = allRoutes.sortWith(_.cost() < _.cost())
      sortedByCost.head.alreadyVisited.last.i match {
        case `destination` => sortedByCost.head
        case _ =>
          val newRoutes = for (neighbourNode <- graph.edgeMap(sortedByCost.head.alreadyVisited.last).toList.filter(!sortedByCost.head.alreadyVisited.contains(_)))
            yield Route(sortedByCost.head.alreadyVisited :+ neighbourNode)
          discoverNewRoutes(newRoutes ++ sortedByCost.tail)
      }
    }

    // :)
    val route = beginJourney(source)
    (route.cost(), route.alreadyVisited)
  }
}