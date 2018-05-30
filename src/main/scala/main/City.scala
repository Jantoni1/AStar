package main

/**
  * Represents a city with coordinates and a name
  *
  * Used as node values in our application
  *
  * @param value tuple of Ints representing (x,y) coordinates
  * @param name name of the city
  */
case class City(value: (Int, Int), name: String) extends Heuristics[(Int, Int), Double] {

  /**
    * Returns a straight line distance between two cities
    * Overrides method from [[main.Heuristics]] trait
    *
    * @param other other city
    * @return distance
    */
  override def heuristicValue(other: Heuristics[(Int, Int), Double]): Double =
  Math.sqrt(Math.pow(other.value._2 - value._2, 2) + Math.pow(other.value._1 - value._1, 2))

  /**
    * Check whether an object is equal to city
    *
    * @param obj other object
    * @return result of comparision
    */
  override def equals(obj: scala.Any): Boolean = obj match {
    case city: City => city.value == value
    case _ => false
  }

  /**
    * Returns hashcode of object
    *
    * @return hashcode value
    */
  override def hashCode(): Int = {
    val state = Seq(value)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}