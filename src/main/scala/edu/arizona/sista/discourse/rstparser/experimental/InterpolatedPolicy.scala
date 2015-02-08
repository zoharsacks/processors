package edu.arizona.sista.discourse.rstparser

import scala.util.Random
import edu.arizona.sista.processors.Document

class InterpolatedPolicy extends Policy {
  var expertProbability: Double = 1
  val expert: ExpertPolicy = new ExpertPolicy
  var learned: LearnedPolicy = _

  def getNextState(currState: State, goldTree: DiscourseTree, doc: Document): State =
    policy.getNextState(currState, goldTree, doc)

  def policy: Policy =
    if (Random.nextDouble() <= expertProbability) expert else learned
}
