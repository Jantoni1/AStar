case class City(x: Int, y: Int, name: String) extends Heuristics {
  override def heuristicValue(other: Heuristics): Double = {
    other match {
      case otherCity: City => Math.sqrt(Math.pow(x - otherCity.x, 2) + Math.pow(y - otherCity.y, 2))
      case _ => throw new ClassCastException
    }
  }
}
