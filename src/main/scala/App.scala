object App {
  def main(args: Array[String]) {
    println("Hello basic-project!")

    val gdansk = City(5, 10, "gdansk")
    val warszawa = City(10, 8, "warszawa")
    val poznan = City(0, 5, "poznan")
    val stalowa = City(12, 3, "stalowa")
    val krakow = City(5, 0, "krakow")

    val graph = new Graph[City, Double](Set.empty[(Node[City], Node[City], Double)], Set.empty[Node[City]])
      .addEdge(Node[City](gdansk), Node[City](poznan), 5)
      .addEdge(Node[City](gdansk), Node[City](warszawa), 15)
      .addEdge(Node[City](poznan), Node[City](warszawa), 2)
      .addEdge(Node[City](poznan), Node[City](krakow), 10)
      .addEdge(Node[City](stalowa), Node[City](krakow), 2)
      .addEdge(Node[City](stalowa), Node[City](warszawa), 3)

    val solver = AStar[City](graph)
    println(solver.routeBetween(krakow, gdansk))
  }
}

