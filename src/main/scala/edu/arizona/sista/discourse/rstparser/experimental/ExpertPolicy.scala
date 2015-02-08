package edu.arizona.sista.discourse.rstparser

import edu.arizona.sista.processors.Document

class ExpertPolicy extends Policy {
  def getNextState(currState: State, goldTree: DiscourseTree, doc: Document): State = {
    val nextStatesWithMergedIndex = getNextStatesWithMergedIndex(currState)
    val (nextStates, mergedIndex) = nextStatesWithMergedIndex.unzip
    val costs = getStatesCosts(nextStates, goldTree)
    val (nextState, cost) = getCheapestState(nextStates, costs)
    nextState
  }
}
