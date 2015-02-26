package edu.arizona.sista.discourse.rstparser.experimental

import edu.arizona.sista.discourse.rstparser._
import edu.arizona.sista.processors.Document
import edu.arizona.sista.discourse.rstparser.Utils.mkGoldEDUs

class ExpertPolicy(val relModel: RelationClassifier) extends Policy {
  def getNextState(currState: State, goldTree: DiscourseTree, doc: Document): State = {
    val edus = mkGoldEDUs(goldTree, doc)
    val nextStatesWithMergedIndex = getNextStatesWithMergedIndex(currState, doc, edus, relModel)
    val (nextStates, mergedIndex) = nextStatesWithMergedIndex.unzip
    val costs = getStatesCosts(nextStates, goldTree)
    val (nextState, cost) = getCheapestState(nextStates, costs)
    nextState
  }
}
