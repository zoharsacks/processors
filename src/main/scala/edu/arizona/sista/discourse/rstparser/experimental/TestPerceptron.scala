package edu.arizona.sista.discourse.rstparser

import scala.util.Random.shuffle
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import edu.arizona.sista.learning.{RVFDataset, Datum, RVFDatum}
import edu.arizona.sista.struct.{Counter, Lexicon}
import breeze.linalg.SparseVector

class DistributedPerceptron(val epochs: Int, val numShards: Int) {
  var finalWeights: SparseVector[Double] = _
  val featureLexicon = new Lexicon[String]

  def train(dataset: RVFDataset[Int, String]): Unit = {
    var avgWeights: SparseVector[Double] = SparseVector.zeros[Double](Integer.MAX_VALUE)
    val indices = Seq.range(0, dataset.size)

    def oneEpoch(shard: Seq[Int]): Future[SparseVector[Double]] = Future {
      var weights = avgWeights.copy
      var avgW = avgWeights.copy
      for (i <- shard) {
        val datum = dataset.mkDatum(i)
        val label = datum.label
        val features = getFeatures(datum.featuresCounter)
        val predLabel = predictLabel(weights, features)
        if (label != predLabel) {
          weights += (if (label > 0) features else -features)
        }
        avgW += weights
      }
      avgW
    }

    for (epoch <- 0 until epochs) {
      val shards = splitShards(shuffle(indices), numShards)
      val futureWeights = Future.sequence(shards map oneEpoch)
      val shardWeights = Await.result(futureWeights, Duration.Inf)
      avgWeights = (avgWeights /: shardWeights) {
        case (lhs, rhs) => lhs + rhs
      }
    }

    finalWeights = avgWeights
  }

  def predictLabel(weights: SparseVector[Double], features: SparseVector[Double]): Int =
    if ((weights dot features) > 0) 1 else -1

  def predictLabel(features: SparseVector[Double]): Int =
    predictLabel(finalWeights, features)

  def predictLabel(features: Counter[String]): Int =
    predictLabel(getFeatures(features))

  def splitShards(indices: Seq[Int], numShards: Int): Seq[Seq[Int]] = {
    var remaining = indices
    val size = indices.size
    val shardSize = size / numShards
    val extraItems = size % numShards
    for (i <- 1 to numShards) yield {
      if (i == numShards) remaining
      else {
        val index = shardSize + (if (i <= extraItems) 1 else 0)
        val (shard, rest) = remaining splitAt index
        remaining = rest
        shard
      }
    }
  }

  def getFeatures(counter: Counter[String]): SparseVector[Double] = {
    val feats = for ((f, v) <- counter.toSeq) yield (featureLexicon.add(f), v)
    val (index, data) = feats.sortBy(_._1).unzip
    new SparseVector(index.toArray, data.toArray, Integer.MAX_VALUE)
  }
}

object TestPerceptron extends App {
  val trainDS = RVFDataset.mkDatasetFromSvmLightFormat("src/test/resources/edu/arizona/sista/learning/classification_train.txt.gz")
  val classifier = new DistributedPerceptron(10, 8)

  println("training ...")
  classifier.train(trainDS)

  println("testing ...")
  val datums = RVFDataset.mkDatumsFromSvmLightFormat("src/test/resources/edu/arizona/sista/learning/classification_test.txt.gz")
  val acc = computeAcc(datums, classifier)
  println("Accuracy: " + acc)

  def computeAcc(datums: Iterable[Datum[Int, String]], classifier: DistributedPerceptron) = {
    var total = 0
    var correct = 0
    for (datum <- datums) {
      val l = classifier.predictLabel(datum.asInstanceOf[RVFDatum[Int, String]].featuresCounter)
      //println(s"prediction: $l")
      if (l == datum.label) correct += 1
      total += 1
    }
    val acc = correct.toDouble / total.toDouble
    acc
  }
}
