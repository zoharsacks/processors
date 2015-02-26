package edu.arizona.sista.discourse.rstparser.experimental

import scala.math.sqrt
import edu.arizona.sista.learning._
import edu.arizona.sista.struct.{Counter, Lexicon}
import breeze.linalg._

class CostSensitiveClassifier[L, F](val epochs: Int, val aggressiveness: Double) {
  require(epochs > 0, "'epochs' should be greater than zero")
  require(aggressiveness > 0, "'aggressiveness' should be greater than zero")

  // import static methods
  import CostSensitiveClassifier._

  // uninitialized lexicons
  var labelLexicon: Lexicon[L] = _
  var featureLexicon: Lexicon[F] = _
  var avgWeights: DenseMatrix[Double] = _

  // train with default costs
  def train(dataset: Dataset[L, F]): Unit =
    train(dataset, mkCostMatrix(dataset))

  def train(dataset: Dataset[L, F], costMatrix: DenseMatrix[Double]): Unit = {
    val numLabels = dataset.numLabels
    val numFeatures = dataset.numFeatures
    val numSamples = dataset.size

    val indices = DenseVector.range(0, numSamples)
    val weights = DenseMatrix.zeros[Double](numLabels, numFeatures)
    avgWeights = weights.copy

    // initialize lexicons
    labelLexicon = Lexicon(dataset.labelLexicon)
    featureLexicon = Lexicon(dataset.featureLexicon)

    for (epoch <- 0 until epochs; i <- shuffle(indices).values) {
      val counter = dataset.featuresCounter(i)
      val trueLabel = dataset.labels(i)
      val feats = counterToSparseVector(counter, numFeatures)
      val predLabel = argmax(weights * feats)
      val predCost = costMatrix(i, predLabel)

      if (predCost > 0) {
        val loss = weights(predLabel, ::) * feats - weights(trueLabel, ::) * feats + sqrt(predCost)
        val learningRate = loss / ((feats dot feats) + (1 / (2 * aggressiveness)))
        val update = feats.t :* learningRate
        weights(trueLabel, ::) :+= update
        weights(predLabel, ::) :-= update
      }

      avgWeights += weights
    }
  }
}

object CostSensitiveClassifier {
  // returns a cost matrix for the given dataset
  // with cost 0 for the right label and cost 1 for everything else
  def mkCostMatrix[L, F](dataset: Dataset[L, F]): DenseMatrix[Double] =
    DenseMatrix.tabulate[Double](dataset.size, dataset.numLabels) {
      (i, j) => if (dataset.labels(i) == j) 0 else 1
    }

  def counterToSparseVector(counter: Counter[Int], length: Int = Integer.MAX_VALUE): SparseVector[Double] = {
    val (index, data) = counter.toSeq.sortBy(_._1).unzip
    new SparseVector(index.toArray, data.toArray, length)
  }
}
