import scala.annotation.tailrec
import scala.math.Numeric
import scala.math.Numeric._
import scala.math.Numeric.Implicits._
import Ordering.Implicits._


object App {
  def main(args: Array[String]) {
    println("Hello basic-project!")
    var graph = new Graph[(Int, Int), Double, City](Set.empty,Set.empty )
    graph = graph.addVertex(Node[City](City((3, 4), "1")))
    graph = graph.addVertex(Node[City](City((4, 4), "2")))
    graph = graph.addVertex(Node[City](City((5, 4), "3")))
    graph = graph.addVertex(Node[City](City((6, 4), "4")))
    graph = graph.addVertex(Node[City](City((7, 4), "5")))
    graph = graph.addVertex(Node[City](City((8, 4), "6")))
    graph = graph.addVertex(Node[City](City((9, 4), "7")))
    graph.vertices.foreach(v1 =>
      graph.vertices.foreach(v2 =>
        if((v1.value.value == (3, 4) && v2.value.value == (9, 4)) || (v1.value.value == (9, 4) && v2.value.value == (3, 4))) {
          graph = graph.addEdge(Edge(v1, v2, 500))
        }
        else if(v1 != v2) {
          graph = graph.addEdge(Edge(v1, v2, 4))
        })  )

    val result = graph.aStar(Node[City](City((3, 4), "1")), Node[City](City((9, 4), "7")), 0)
    result.foreach(vertex => println(vertex.value.value))

  }



}