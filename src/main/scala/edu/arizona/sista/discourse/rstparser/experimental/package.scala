package edu.arizona.sista.discourse

import scala.util.Random
import edu.arizona.sista.discourse.rstparser.RelationDirection

package object rstparser {
  type State = Seq[DiscourseTree]

  def getNextStatesWithMergedIndex(state: State): Seq[(State, Int)] =
    for (i <- 0 to state.size - 2) yield {
      val children = state.slice(i, i + 2).toArray
      val label = "DUMMY_LABEL"
      val direction = RelationDirection.None
      val node = new DiscourseTree(label, direction, children)
      val nextState = state.take(i) ++ Seq(node) ++ state.drop(i + 2)
      (nextState, i)
    }

  def getStatesCosts(states: Seq[State], gold: DiscourseTree): Seq[Double] =
    states map (s => StructureScorer.loss(gold, s))

  // returns all next states with minimum cost
  def getCheapestStates(states: Seq[State], costs: Seq[Double]): (Seq[State], Double) = {
    val minCost = costs.min
    val cheapestStates = for ((state, cost) <- states zip costs if cost == minCost) yield state
    (cheapestStates, minCost)
  }

  // returns the best next state
  // if there are several equally good then pick one randomly
  def getCheapestState(states: Seq[State], costs: Seq[Double]): (State, Double) = {
    val (cheapestStates, cost) = getCheapestStates(states, costs)
    val i = Random.nextInt(cheapestStates.size)
    (cheapestStates(i), cost)
  }


  // linear algebra stuff

  type DenseVector = Array[Double]
  type SparseVector = Map[Int, Double]

  def dotProduct(vec1: DenseVector, vec2: DenseVector): Double = {
    require(vec1.size == vec2.size, "vectors should have the same dimensions")
    val prods = for ((v1, v2) <- vec1 zip vec2) yield v1 * v2
    prods.sum
  }

  def dotProduct(sparse: SparseVector, dense: DenseVector): Double = {
    val prods = for ((i, v) <- sparse) yield v * dense(i)
    prods.sum
  }

  def dotProduct(dense: DenseVector, sparse: SparseVector): Double =
    dotProduct(sparse, dense)

  def dotProduct(v1: SparseVector, v2: SparseVector): Double = {
    val indices = v1.keySet ++ v2.keySet
    val prods = for (i <- indices) yield v1.getOrElse(i, 0.0) * v2.getOrElse(i, 0.0)
    prods.sum
  }

  def vectorAdd(v1: SparseVector, v2: SparseVector): SparseVector = {
    val indices = v1.keySet ++ v2.keySet
    val elems = for (i <- indices) yield i -> (v1.getOrElse(i, 0.0) + v2.getOrElse(i, 0.0))
    elems.toMap
  }

  def vectorSubtract(v1: SparseVector, v2: SparseVector): SparseVector = {
    val indices = v1.keySet ++ v2.keySet
    val elems = for (i <- indices) yield i -> (v1.getOrElse(i, 0.0) - v2.getOrElse(i, 0.0))
    elems.toMap
  }

  def sparseToDense(v: SparseVector): DenseVector =
    Array.tabulate(v.keySet.max + 1)(v withDefaultValue 0)
}
