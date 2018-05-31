package main

object App {
  def main(args: Array[String]) {
    val gdansk = City((5, 10), "gdansk")
    val warszawa = City((10, 8), "warszawa")
    val lodz = City((0, 5), "lodz")
    val lublin = City((12, 3), "lublin")
    val krakow = City((5, 0), "krakow")
    val gdynia = City((4, 9), "gdynia")

    val graph = new Graph[(Int, Int), Double, City](Set.empty, Set.empty)
      .addEdge(Edge(Node[City](gdansk), Node[City](lodz), 50))
      .addEdge(Edge(Node[City](gdansk), Node[City](warszawa), 150))
      .addEdge(Edge(Node[City](lodz), Node[City](gdynia), 20))
      .addEdge(Edge(Node[City](lodz), Node[City](krakow), 100))
      .addEdge(Edge(Node[City](lodz), Node[City](warszawa), 20))
      .addEdge(Edge(Node[City](warszawa), Node[City](lublin), 30))
      .addEdge(Edge(Node[City](krakow), Node[City](lublin), 20))

    println(graph.routeBetween(krakow, gdynia))
    println(graph.routeBetween(lodz, krakow))
  }
}