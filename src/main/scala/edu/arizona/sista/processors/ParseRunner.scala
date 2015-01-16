package edu.arizona.sista.processors

import edu.arizona.sista.processors.bionlp.BioNLPProcessor
import edu.arizona.sista.processors.corenlp.CoreNLPProcessor
import edu.arizona.sista.processors.fastnlp.FastNLPProcessor

/**
 * External API for running different discourse parsers for visualization.
 * Written By: Tom Hicks. 1/15/2015.
 * Last Modified: Initial creation.
 */
object ParseRunner {

  def parseText(text: String): ParseResults = parseText(text, useProcessor = "core")

  def parseText(text: String, useProcessor: String): ParseResults = {
    // default processor is the slow but better discourse parser
    var processor: Processor = new CoreNLPProcessor(withDiscourse = true)

    if (useProcessor == "fast")             // fast but slightly worse discourse parser
      processor = new FastNLPProcessor(withDiscourse = true)

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

}


class ParseResults(val text: String, val dTrees: Array[String])
