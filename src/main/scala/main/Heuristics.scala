package main

/**
  * Trait allowing calculation of heuristic value between two objects
  *
  * Any class used as value in [[main.Node]] needs to extend this trait in order to work with A* algorithm
  *
  * @tparam T type of value used to calculate heuristic value
  * @tparam V type of heuristic value
  */
trait Heuristics[T, V] {

  /**
    * Returns heuristic value between two objects
    *
    * @param other other object
    * @return heuristic value
    */
  def heuristicValue(other: Heuristics[T, V]): V
  val value : T
}