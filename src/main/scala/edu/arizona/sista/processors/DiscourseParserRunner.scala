package edu.arizona.sista.processors

import edu.arizona.sista.processors.bionlp.BioNLPProcessor
import edu.arizona.sista.processors.corenlp.CoreNLPProcessor
import edu.arizona.sista.processors.fastnlp.FastNLPProcessor

/**
 * External API for running different discourse parsers for visualization.
 * Written By: Tom Hicks. 1/15/2015.
 * Last Modified: Create own annotate method to log time for all phases.
 */
class DiscourseParserRunner (useProcessor:String = "core") {
  val processor:Processor =
    if (useProcessor == "fast")             // fast but slightly worse discourse parser
      new FastNLPProcessor(withDiscourse = true)
    else                                    // default: slow but better discourse parser
      new CoreNLPProcessor(withDiscourse = true)


  def parseText (text: String): DiscourseParserResults = {
    // create and annotate a document using the selected processor
    val start = System.currentTimeMillis()
    val doc = myAnnotate(processor.mkDocument(text))  // call for custom processing
    // val doc = processor.annotate(text)
    val stop = System.currentTimeMillis()
    val elapsed = stop - start
    println(this.toString()+": [ANNOT]: %5d, %d, %d".format(elapsed, start, stop))

    doc.discourseTree.foreach(dt => {
      println(this.toString()+": Discourse tree from processor.annotate (with coRef)")
      println(dt.toString())
    })

    // return information from discourse trees as an array of JSON strings:
    new DiscourseParserResults(text, elapsed, discTrees(doc), synTrees(doc))
  }

  def discTrees (doc: Document): Array[String] = {
    val allTrees = doc.discourseTree map { dTree =>
      dTree.visualizerJSON()
    }
    allTrees.toArray
  }

  def synTrees (doc: Document): Array[String] = {
    val allTrees = doc.sentences map { s =>
      s.syntacticTree.getOrElse("()").toString()
    }
    allTrees.toArray
  }


  // this alternate annotate method can be called from above to bypass co-reference processing
  def myAnnotate (doc:Document): Document = {
    var t = Map[String, java.lang.Long]()

    t = timeIt { processor.tagPartsOfSpeech(doc) }
    println(this.toString()+":   [POS]: %5d, %d, %d".format(t("elapsed"), t("start"), t("stop")))

    t = timeIt { processor.lemmatize(doc) }
    println(this.toString()+": [Lemma]: %5d, %d, %d".format(t("elapsed"), t("start"), t("stop")))

    t = timeIt { processor.recognizeNamedEntities(doc) }
    println(this.toString()+":   [NER]: %5d, %d, %d".format(t("elapsed"), t("start"), t("stop")))

    t = timeIt { processor.parse(doc) }
    println(this.toString()+": [Parse]: %5d, %d, %d".format(t("elapsed"), t("start"), t("stop")))

    t = timeIt { processor.chunking(doc) }
    println(this.toString()+": [Chunk]: %5d, %d, %d".format(t("elapsed"), t("start"), t("stop")))

    t = timeIt { processor.labelSemanticRoles(doc) }
    println(this.toString()+": [Roles]: %5d, %d, %d".format(t("elapsed"), t("start"), t("stop")))

    t = timeIt { processor.resolveCoreference(doc) }
    println(this.toString()+": [CoRef]: %5d, %d, %d".format(t("elapsed"), t("start"), t("stop")))

    t = timeIt { processor.discourse(doc) }
    println(this.toString()+": [DiscP]: %5d, %d, %d".format(t("elapsed"), t("start"), t("stop")))

    doc.clear()
    doc
  }


  // return a tuple of start time, stop time, and elapsed time for the given block
  def timeIt (it: => Unit): Map[String, java.lang.Long] = {
    val start = System.currentTimeMillis()
    it
    val stop = System.currentTimeMillis()
    return Map("start"->start, "stop"->stop, "elapsed"->(stop-start))
  }


  override def toString:String = {
    return "<%s:%s>".format(super.getClass().getSimpleName(), processor.getClass.getSimpleName())
  }
}


class DiscourseParserResults(val text:String,
                             val annoTime:java.lang.Long,
                             val dTrees:Array[String],
                             val synTrees:Array[String])
