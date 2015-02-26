package edu.arizona.sista.discourse.rstparser

import scala.util.Random
import edu.arizona.sista.processors.Document

package object experimental {
  type State = Seq[DiscourseTree]

  def getNextStatesWithMergedIndex(state: State, doc: Document, edus: Array[Array[(Int, Int)]], relationModel: RelationClassifier): Seq[(State, Int)] =
    for (i <- 0 to state.size - 2) yield {
      val children = state.slice(i, i + 2).toArray
      val d = relationModel.mkDatum(children(0), children(1), doc, edus, StructureClassifier.NEG)
      val ld = relationModel.classOf(d)
      val (label, dir) = relationModel.parseLabel(ld)
      val node = new DiscourseTree(label, dir, children)
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
