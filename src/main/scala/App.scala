
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

    val city1 = new City(2, 2, "warszawa")
    val city2 = new City(3, 0, "radom")
    println(city1.heuristicValue(city2))
  }
}

