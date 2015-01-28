package edu.arizona.sista.processors

import edu.arizona.sista.processors.bionlp.BioNLPProcessor
import edu.arizona.sista.processors.corenlp.CoreNLPProcessor
import edu.arizona.sista.processors.fastnlp.FastNLPProcessor

/**
 * External API for running different discourse parsers for visualization.
 * Written By: Tom Hicks. 1/15/2015.
 * Last Modified: Rename discourse parser runner class, add syntax trees to results.
 */
class DiscourseParserRunner (useProcessor:String = "core") {
  val processor:Processor =
    if (useProcessor == "fast")             // fast but slightly worse discourse parser
      new FastNLPProcessor(withDiscourse = true)
    else                                    // default: slow but better discourse parser
      new CoreNLPProcessor(withDiscourse = true)


  def parseText(text: String): DiscourseParserResults = {
    // annotate the document using the selected processor
    val doc = processor.annotate(text)
    // return information from discourse trees as an array of JSON strings:
    new DiscourseParserResults(text, discTrees(doc), synTrees(doc))
  }

  def discTrees(doc: Document): Array[String] = {
    val allTrees = doc.discourseTree map { dTree =>
      dTree.visualizerJSON()
    }
    allTrees.toArray
  }

  def synTrees(doc: Document): Array[String] = {
    val allTrees = doc.sentences map { s =>
      s.syntacticTree.getOrElse("()").toString()
    }
    allTrees.toArray
  }

  override def toString:String = {
    return "<%s: %s>".format(super.toString(), useProcessor)
  }
}


class DiscourseParserResults(val text:String, val dTrees:Array[String], val synTrees:Array[String])
