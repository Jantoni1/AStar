trait Heuristics[T, V] {
  def heuristicValue(other: Heuristics[T, V]): V
  val value : T
}