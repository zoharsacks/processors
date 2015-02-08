package edu.arizona.sista.discourse.rstparser

import edu.arizona.sista.processors.Document
import edu.arizona.sista.discourse.rstparser.Utils.mkGoldEDUs

class LearnedPolicy(val weights: DenseVector, val corpusStats: CorpusStats) extends Policy {
  val featureExtractor = new FeatureExtractor

  def getNextState(currState: State, goldTree: DiscourseTree, doc: Document): State = {
    val edus = mkGoldEDUs(goldTree, doc)
    val scores = 0 to currState.size - 2 map { i =>
      val features = getFeatures(currState, i, doc, edus, corpusStats)
      dotProduct(weights, features)
    }
    val merge = scores indexOf scores.max
    val children = currState.slice(merge, merge + 2).toArray
    val label = "DUMMY_LABEL"
    val direction = RelationDirection.None
    val node = new DiscourseTree(label, direction, children)
    val nextState = currState.take(merge) ++ Seq(node) ++ currState.drop(merge + 2)
    nextState
  }

  def getFeatures(state: State,
                  merge: Int,
                  doc: Document,
                  edus: Array[Array[(Int, Int)]],
                  corpusStats: CorpusStats): SparseVector = {
    val left = state(merge)
    val right = state(merge + 1)
    featureExtractor.getFeatures(left, right, doc, edus, corpusStats, "DUMMY_LABEL")
  }

  def parseWithGoldEDUs(tree: DiscourseTree, doc: Document): DiscourseTree =
    getCompletePath(tree, doc).last.head
}
