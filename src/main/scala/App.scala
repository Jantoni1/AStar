import scala.annotation.tailrec
import scala.math.Numeric
import scala.math.Numeric._
import scala.math.Numeric.Implicits._
import Ordering.Implicits._

case class Town(value: (Int, Int), name: String) extends Ordered[Town] with  Heuristics[(Int, Int), Double] {
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

  override def compareTo(that: Town): Int = {
    if (value._1 > that.value._1 || (value._1 == that.value._1 && value._2 > that.value._2)) 1
    else if (value._1 == that.value._1 && value._2 == that.value._2) 0
    else -1
  }

  override def compare(that: Town): Int = compareTo(that)

}



object App {
  def main(args: Array[String]) {
    println("Hello basic-project!")
//    val l1= List(0, 3, 6, 12, 14, 15, 16, 17)
//    val l2 = l1.zip(l1.tail)
//    l2.foreach(println)
    var graph = new Graph[(Int, Int), Double, Town](Set.empty,Set.empty )
    graph = graph.addVertex(Node( Town((3, 4), "jeden")))
    graph = graph.addVertex(Node[Town]( Town((4, 4), "dwa")))
    graph = graph.addVertex(Node[Town]( Town((5, 4), "trzy")))
    graph = graph.addVertex(Node[Town]( Town((6, 4), "cztery")))
    graph = graph.addVertex(Node[Town]( Town((7, 4), "piec")))
    graph = graph.addVertex(Node[Town]( Town((8, 4), "szesc")))
    graph = graph.addVertex(Node[Town]( Town((9, 4), "siedem")))
    graph.vertices.foreach(v1 =>
      graph.vertices.foreach(v2 =>
        if((v1.value.value == (3, 4) && v2.value.value == (9, 4)) || (v1.value.value == (9, 4) && v2.value.value == (3, 4))) {
          graph = graph.addEdge(Edge(v1, v2, 500))
        }
        else if(v1 != v2) {
        graph = graph.addEdge(Edge(v1, v2, 4))
      })  )

    val result = graph.routeBetween(Town((3,4), "jeden"), Town((9,4), "siedem"))
    println(result._1)
    result._2.foreach(println)
  }



}

