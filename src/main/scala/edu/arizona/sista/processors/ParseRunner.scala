package edu.arizona.sista.processors

import edu.arizona.sista.processors.bionlp.BioNLPProcessor
import edu.arizona.sista.processors.corenlp.CoreNLPProcessor
import edu.arizona.sista.processors.fastnlp.FastNLPProcessor

/**
 * External API for running different discourse parsers for visualization.
 * Written By: Tom Hicks. 1/15/2015.
 * Last Modified: Redo this object as a class.
 */
class ParseRunner (useProcessor:String = "core") {
  val processor:Processor =
    if (useProcessor == "fast")             // fast but slightly worse discourse parser
      new FastNLPProcessor(withDiscourse = true)
    else                                    // default: slow but better discourse parser
      new CoreNLPProcessor(withDiscourse = true)


  def parseText(text: String): ParseResults = {
    // annotate the document using the selected processor
    val doc = processor.annotate(text)
    // return information from discourse trees as an array of JSON strings:
    new ParseResults(text, discTrees(doc))
  }

  def discTrees(doc: Document): Array[String] = {
    val allTrees = doc.discourseTree map { dTree =>
      dTree.visualizerJSON()
    }
    allTrees.toArray
  }

  override def toString:String = {
    return "<%s: %s>".format(super.toString(), useProcessor)
  }
}


class ParseResults(val text: String, val dTrees: Array[String])
