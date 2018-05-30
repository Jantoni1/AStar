import math.sqrt
import math.pow

class City(val x: Int, val y: Int, val name: String) extends Heuristics {
  override def heuristicValue(other: Heuristics): Double = {
    other match {
      case otherCity: City => sqrt(pow(x - otherCity.x, 2) + pow(y - otherCity.y, 2))
      case _ => throw new ClassCastException
    }
  }
}
