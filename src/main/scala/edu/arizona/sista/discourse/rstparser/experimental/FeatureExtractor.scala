package edu.arizona.sista.discourse.rstparser

import edu.arizona.sista.struct.{Counter, Lexicon}
import edu.arizona.sista.processors.Document
import breeze.linalg.SparseVector

class FeatureExtractor {
  val featureExtractor = new RelationFeatureExtractor

  val lexicon = new Lexicon[String]
  def numFeatures = lexicon.size

  def getFeatures(left: DiscourseTree,
                  right: DiscourseTree,
                  doc: Document,
                  edus: Array[Array[(Int, Int)]],
                  corpusStats: CorpusStats,
                  label: String): SparseVector[Double] = {
    val feats = featureExtractor.mkFeatures(left, right, doc, edus, corpusStats, label)
    getFeatures(feats)
  }

  def getFeatures(counter: Counter[String]): SparseVector[Double] = {
    val feats = for ((f, v) <- counter.toSeq) yield (lexicon.add(f), v)
    val (index, data) = feats.sortBy(_._1).unzip
    new SparseVector(index.toArray, data.toArray, Integer.MAX_VALUE)
  }
}
