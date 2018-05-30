case class City(value: (Int, Int), name: String) extends  Heuristics[(Int, Int), Double] {
  override def heuristicValue(other: Heuristics[(Int, Int), Double]): Double =
  Math.sqrt(Math.pow(other.value._2 - value._2, 2) + Math.pow(other.value._1 - value._1, 2))

  override def equals(obj: scala.Any): Boolean = obj match {
    case town: City => town.value == value
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(value)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}