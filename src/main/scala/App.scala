
object App {
  def main(args: Array[String]) {
    println("Hello basic-project!")
    var graph = new Graph[Int, Double](Set.empty[(Node[Int], Node[Int], Double)],Set.empty[Node[Int]] )
    graph = graph.addVertex(Node[Int](3))
    graph = graph.addVertex(Node[Int](4))
    graph = graph.addEdge((Node[Int](3), Node[Int](4), 5))
    graph = graph.removeVertex(Node[Int](3))
    graph.vertices.foreach(v => println(v.i))
    graph.edges.foreach(e => println(e._3))

    val destination = Node[(Int, Int)](3, 3)
    var source = Node[(Int, Int)](4, 4)

    var cost = 0
    var heuristics = value(source.i, destination.i)

  }

  def value(xy1 : (Int, Int), xy2 : (Int, Int)) : Double = Math.sqrt((xy2._2 - xy1._2) * (xy2._1 - xy1._1))

  def aStar(graph : Graph[(Int, Int), Double], source : Node[(Int, Int)], destination : Node[(Int, Int)],
            cost : Double, heuristics : Double) : List[Node[(Int, Int)]]  = {
    if(source == destination) {
      return List(destination)
    }
    val newNode = graph.edgeMap.apply(source).minBy(node => {cost + graph.getEdge(source, node)._3 + value(node.i, destination.i) })//min(node => )
    newNode :: aStar(graph, newNode, destination, cost, heuristics)
  }
}

