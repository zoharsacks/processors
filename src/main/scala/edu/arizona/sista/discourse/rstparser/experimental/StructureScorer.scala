package edu.arizona.sista.discourse.rstparser

import edu.arizona.sista.struct.Interval

object StructureScorer {
  def loss(goldTree: DiscourseTree, predictedTree: DiscourseTree): Double = {
    val (goldRanges, predictedRanges) = getEDUIntervals(goldTree, Seq(predictedTree))
    val falsePositives = predictedRanges diff goldRanges
    val falseNegatives = goldRanges diff predictedRanges
    falsePositives.size + falseNegatives.size
  }

  // optimal approximation loss (expected regret)
  def loss(goldTree: DiscourseTree, partialPrediction: Seq[DiscourseTree]): Double = {
    val (goldRanges, predictedRanges) = getEDUIntervals(goldTree, partialPrediction)
    val falsePositives = predictedRanges diff goldRanges
    // get false negatives
    // these are all impossible EDUIntervals given the current solution, because of boundary overlapping
    // if a gold EDUInterval is not in the partial solution it is *not* considered a false negative
    // because we may get it right in the future
    val falseNegatives = falsePositives flatMap (fp => goldRanges filter fp.overlaps)
    falsePositives.size + falseNegatives.size
  }

  private case class EDU(sentence: Int, start: Int, end: Int) {
    val interval = Interval(start, end)
    val size: Int = interval.size
  }

  private case class EDUInterval(start: Int, end: Int) {
    val interval = Interval(start, end)
    val size: Int = interval.size
    def overlaps(that: EDUInterval): Boolean =
      this.interval.allenOverlaps(that.interval) || that.interval.allenOverlaps(this.interval)
  }

  private def mkEDUs(node: DiscourseTree): Seq[EDU] = {
    if (node.isTerminal) {
      Seq(EDU(node.firstToken.sentence, node.firstToken.token, node.lastToken.token + 1))
    } else {
      node.children flatMap mkEDUs
    }
  }

  private def getEDUIntervals(goldTree: DiscourseTree, partialPrediction: State): (Set[EDUInterval], Set[EDUInterval]) = {
    // get all EDUs for the gold tree
    // we are assuming that the prediction uses gold EDUs
    val edus = mkEDUs(goldTree)
    // get a function that gets a tree and returns its EDUInterval in the `edus` sequence
    val mkEDUInterval = mkEDUIntervalFactory(edus) _
    // return function that gets a tree and returns all EDUIntervals for it and its descendants
    val allEDUIntervals = allEDUIntervalsFactory(mkEDUInterval) _
    // get all EDUIntervals for gold tree
    val goldRanges = allEDUIntervals(goldTree)
    // get all EDUIntervals for predicted tree
    val predictedRanges = partialPrediction.toSet flatMap allEDUIntervals
    // ignore ranges with a single EDU
    (goldRanges.filter(_.size > 1), predictedRanges.filter(_.size > 1))
  }

  private def mkEDUIntervalFactory(docEDUs: Seq[EDU])(node: DiscourseTree) = {
    val nodeEDUs = mkEDUs(node)
    EDUInterval(docEDUs.indexOf(nodeEDUs.head), docEDUs.indexOf(nodeEDUs.last) + 1)
  }

  private def allEDUIntervalsFactory(mkEDUInterval: DiscourseTree => EDUInterval)(node: DiscourseTree): Set[EDUInterval] = {
    if (node.isTerminal) Set(mkEDUInterval(node))
    else node.children.toSet.flatMap(allEDUIntervalsFactory(mkEDUInterval)) + mkEDUInterval(node)
  }
}
