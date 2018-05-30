import scala.annotation.tailrec
import scala.math.Numeric
import scala.math.Numeric._
import scala.math.Numeric.Implicits._
import Ordering.Implicits._

class Town(val value : (Int, Int)) extends  Heuristics[(Int, Int), Double] {
  override def heuristicValue(other: Heuristics[(Int, Int), Double]): Double =
    Math.sqrt(Math.pow(other.value._2 - value._2, 2) + Math.pow(other.value._1 - value._1, 2))

  override def equals(obj: scala.Any): Boolean = obj match {
    case town: Town => town.value == value
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(value)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}



object App {
  def main(args: Array[String]) {
    println("Hello basic-project!")
    var graph = new Graph[(Int, Int), Double, Town](Set.empty,Set.empty )
    graph = graph.addVertex(Node[Town](new Town(3, 4)))
    graph = graph.addVertex(Node[Town](new Town(4, 4)))
    graph = graph.addVertex(Node[Town](new Town(5, 4)))
    graph = graph.addVertex(Node[Town](new Town(6, 4)))
    graph = graph.addVertex(Node[Town](new Town(7, 4)))
    graph = graph.addVertex(Node[Town](new Town(8, 4)))
    graph = graph.addVertex(Node[Town](new Town(9, 4)))
    graph.vertices.foreach(v1 =>
      graph.vertices.foreach(v2 =>
        if((v1.value.value == (3, 4) && v2.value.value == (9, 4)) || (v1.value.value == (9, 4) && v2.value.value == (3, 4))) {
          graph = graph.addEdge(Edge(v1, v2, 500))
        }
        else if(v1 != v2) {
        graph = graph.addEdge(Edge(v1, v2, 4))
      })  )

    val result = graph.aStar(Node(new Town(3,4)), Node(new Town(9,4)), 0)
    result.foreach(vertex => println(vertex.value.value))

  }



}

