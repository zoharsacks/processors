package edu.arizona.sista.discourse.rstparser


object Trainer extends App {
  val trainDirName = "/work/marcov/data/RST_cached_preprocessing/rst_train"
  val testDirName = "/work/marcov/data/RST_cached_preprocessing/rst_test"
  val dependencySyntax = true
  val processor = CacheReader.getProcessor(dependencySyntax)

  val rstparser = RSTParser.loadFrom(RSTParser.DEFAULT_DEPENDENCYSYNTAX_MODEL_PATH)

  var policy: LearnedPolicy = _

  println("training ...")
  train()
  println("testing ...")
  test()

  def train(): Unit = {
    val (treedocs, corpusStats) = RSTParser.mkTrees(trainDirName, processor)
    val structurer = new Structurer(3, 0.4, rstparser.relModel)
    policy = structurer.train(treedocs.toIndexedSeq, corpusStats, 8)
  }

  def test(): Unit = {
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
