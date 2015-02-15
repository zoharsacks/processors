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
}
