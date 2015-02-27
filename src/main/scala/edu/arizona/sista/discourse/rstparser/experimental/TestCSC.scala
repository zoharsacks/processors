package edu.arizona.sista.discourse.rstparser.experimental

import scala.util.Random.shuffle
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import edu.arizona.sista.learning.{RVFDataset, Datum, RVFDatum}
import edu.arizona.sista.struct.{Counter, Lexicon}
import breeze.linalg.SparseVector

object TestCSC extends App {
  val trainDS = RVFDataset.mkDatasetFromSvmLightFormat("src/test/resources/edu/arizona/sista/learning/classification_train.txt.gz")
  val classifier = new CostSensitiveClassifier[Int, String](3, 1)

  println("training ...")
  classifier.train(trainDS)

  println("testing ...")
  val datums = RVFDataset.mkDatumsFromSvmLightFormat("src/test/resources/edu/arizona/sista/learning/classification_test.txt.gz")
  val acc = computeAcc(datums, classifier)
  println("Accuracy: " + acc)

  def computeAcc(datums: Iterable[Datum[Int, String]], classifier: CostSensitiveClassifier[Int, String]) = {
    var total = 0
    var correct = 0
    for (datum <- datums) {
      val l = classifier.predictLabel(datum.asInstanceOf[RVFDatum[Int, String]])
      //println(s"prediction: $l")
      if (l == datum.label) correct += 1
      total += 1
    }
    val acc = correct.toDouble / total.toDouble
    acc
  }
}
