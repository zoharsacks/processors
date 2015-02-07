package edu.arizona.sista.processors.visualizer

import scala.collection.JavaConverters._

import edu.arizona.sista.processors._
import edu.arizona.sista.processors.bionlp.BioNLPProcessor
import edu.arizona.sista.processors.corenlp.CoreNLPProcessor
import edu.arizona.sista.processors.fastnlp.FastNLPProcessor
import edu.arizona.sista.processors.visualizer._  // import package object

/**
 * External API for running different discourse parsers for visualization.
 * Written By: Tom Hicks. 1/15/2015.
 * Last Modified: Return map of timings in moved discourse parser results.
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
    // val doc = processor.annotate(text)
    // var timings:TimingsType = TimingsType()
    var (doc, timings) = myAnnotate(processor.mkDocument(text)) // call for custom processing
    val stop = System.currentTimeMillis()
    val elapsed = stop - start
    timings ++= List("ANNOT.start"-> start, "ANNOT.stop"->stop, "ANNOT.elapsed"->elapsed)
//    println(this.toString()+": [ANNOT]: %5d, %d, %d".format(elapsed, start, stop))

    doc.discourseTree.foreach(dt => {
      println(this.toString()+": Discourse tree from processor.annotate (with coRef)")
      println(dt.toString())
    })

    // return information from discourse trees as an array of JSON strings:
    new DiscourseParserResults(text, timings.asJava, discTrees(doc), synTrees(doc))
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


  // this alternate annotate method can be called from above to customize processing
  def myAnnotate (doc:Document): (Document, TimingsType) = {
    var t:TimingsType = TimingsType()

    t ++= timeIt("POS",   { processor.tagPartsOfSpeech(doc) })
    t ++= timeIt("Lemma", { processor.lemmatize(doc) })
    t ++= timeIt("NER",   { processor.recognizeNamedEntities(doc) })
    t ++= timeIt("Parse", { processor.parse(doc) })
    t ++= timeIt("Chunk", { processor.chunking(doc) })
    t ++= timeIt("Roles", { processor.labelSemanticRoles(doc) })
    t ++= timeIt("CoRef", { processor.resolveCoreference(doc) })
    t ++= timeIt("DiscP", { processor.discourse(doc) })

    doc.clear()
    return (doc, t)
  }


  // return a map of start time, stop time, and elapsed time for the given block
  def timeIt (prefix:String, it: => Unit): TimingsType = {
    val start = System.currentTimeMillis()
    it
    val stop = System.currentTimeMillis()
    return scala.collection.mutable.Map((s"${prefix}.start")->start,
                                        (s"${prefix}.stop")->stop,
                                        (s"${prefix}.elapsed")->(stop-start))
  }


  override def toString:String = {
    return "<%s:%s>".format(super.getClass().getSimpleName(), processor.getClass.getSimpleName())
  }
}


class DiscourseParserResults(val text:String,
                             val timings:java.util.Map[String, java.lang.Long],
                             val dTrees:Array[String],
                             val synTrees:Array[String])
