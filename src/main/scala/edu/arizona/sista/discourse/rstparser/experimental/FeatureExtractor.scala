package edu.arizona.sista.discourse.rstparser

import edu.arizona.sista.struct.{Counter, Lexicon}
import edu.arizona.sista.processors.Document

class FeatureExtractor {
  val featureExtractor = new RelationFeatureExtractor

  val lexicon = new Lexicon[String]
  def numFeatures = lexicon.size

  def getFeatures(left: DiscourseTree,
                  right: DiscourseTree,
                  doc: Document,
                  edus: Array[Array[(Int, Int)]],
                  corpusStats: CorpusStats,
                  label: String): SparseVector = this.synchronized {
    val feats = featureExtractor.mkFeatures(left, right, doc, edus, corpusStats, label)
    getFeatures(feats)
  }

  def getFeatures(counter: Counter[String]): SparseVector =
    for ((f, v) <- counter.toSeq.toMap) yield (getOrCreateFeatureIndex(f), v)

  def getOrCreateFeatureIndex(s: String): Int = lexicon.get(s) match {
    case Some(i) => i
    case None => lexicon.add(s)
  }
}
