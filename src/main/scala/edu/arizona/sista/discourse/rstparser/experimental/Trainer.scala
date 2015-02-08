package edu.arizona.sista.discourse.rstparser


object Trainer extends App {
  val trainDirName = "/Users/marcov/data/RST_cached_preprocessing/rst_train"
  val testDirName = "/Users/marcov/data/RST_cached_preprocessing/rst_test"
  val dependencySyntax = true
  val processor = CacheReader.getProcessor(dependencySyntax)

  var policy: LearnedPolicy = _

  train()
  test()

  def train(): Unit = {
    println("training ...")
    val (treedocs, corpusStats) = RSTParser.mkTrees(trainDirName, processor)
    val structurer = new Structurer(3, 0.4)
    policy = structurer.train(treedocs.toIndexedSeq, corpusStats, 6)
  }

  def test(): Unit = {
    println("testing ...")
    val scorer = new DiscourseScorer
    val structScoreGold = new DiscourseScore()
    val (treedocs, corpusStats) = RSTParser.mkTrees(testDirName, processor)
    for ((tree, doc) <- treedocs) {
      val sys = policy.parseWithGoldEDUs(tree, doc)
      scorer.score(sys, tree, structScoreGold, ScoreType.OnlyStructure)
    }
    println("STRUCT SCORE (with gold EDUs):\n" + structScoreGold)
  }
}
