import main._
import org.scalatest.FunSuite

class GraphTests extends FunSuite {

  case class IntNode(value: Int) extends Heuristics[Int, Int] {
    override def heuristicValue(other: Heuristics[Int, Int]): Int = Math.abs(value - other.value)
  }

  val graph = new Graph[Int, Int, IntNode](Set.empty, Set.empty)

  test("givenEmptyConstructorGraphShouldBeEmpty") {
    assert(graph.vertices.isEmpty)
  }

  test("addingNewNodeShouldIncreaseSize") {
    val added = graph.addVertex(Node[IntNode](IntNode(2)))
    assert(added.vertices.nonEmpty)
  }

  test("addingExistingNodeShouldNotIncreaseSize") {
    val added = graph.addVertex(Node[IntNode](IntNode(1)))
    val same = graph.addVertex(Node[IntNode](IntNode(1)))
    assert(added.vertices.equals(same.vertices))
  }

  test("removingExistingNodeShouldDecreaseSize") {
    val added = graph.addVertex(Node[IntNode](IntNode(3)))
    assert(added.vertices.size.equals(1))
    val removed = added.removeVertex(Node[IntNode](IntNode(3)))
    assert(removed.vertices.isEmpty)
  }

  test("removingNonExistingNodeShouldNotChangeAnything") {
    val added = graph.addVertex(Node[IntNode](IntNode(3))).addVertex(Node[IntNode](IntNode(7)))
    val removed = added.removeVertex(Node[IntNode](IntNode(5)))
    assert(added.vertices.equals(removed.vertices))
  }

  test("addingEdgeShouldIncreaseSize") {
    val node1 = Node[IntNode](IntNode(3))
    val node2 = Node[IntNode](IntNode(4))
    val added = graph.addVertex(node1).addVertex(node2).addEdge(Edge(node1, node2, 10))
    assert(added.edges.nonEmpty)
  }

  test("addingEdgeWithInexistentNodesShouldAddNodes") {
    val node1 = Node[IntNode](IntNode(35))
    val node2 = Node[IntNode](IntNode(45))
    val added = graph.addEdge(Edge(node1, node2, 10))
    assert(added.vertices.size.equals(2))
  }

  test("removingEdgeShouldDecreaseSize") {
    val node1 = Node[IntNode](IntNode(3))
    val node2 = Node[IntNode](IntNode(4))
    val edge = Edge(node1, node2, 1)
    val added = graph.addEdge(edge)
    val removed = added.removeEdge(edge)
    assert(added.edges.nonEmpty)
    assert(removed.edges.isEmpty)
  }

  test("edgeShouldContainAddedNodes") {
    val node1 = Node[IntNode](IntNode(3))
    val node2 = Node[IntNode](IntNode(4))
    val node3 = Node[IntNode](IntNode(5))
    val edge = Edge(node1, node2, 1)
    assert(edge.contains(node1))
    assert(edge.contains(node2))
    assert(!edge.contains(node3))
  }

  test("getEdgeShouldReturnDesiredEdge") {
    val node1 = Node[IntNode](IntNode(3))
    val node2 = Node[IntNode](IntNode(4))
    val edge = Edge(node1, node2, 1)
    val added = graph.addEdge(edge)
    assert(edge.equals(added.getEdge(node1, node2)))
  }
}