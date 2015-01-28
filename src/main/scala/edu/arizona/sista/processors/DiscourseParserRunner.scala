package edu.arizona.sista.processors

import edu.arizona.sista.processors.bionlp.BioNLPProcessor
import edu.arizona.sista.processors.corenlp.CoreNLPProcessor
import edu.arizona.sista.processors.fastnlp.FastNLPProcessor

/**
 * External API for running different discourse parsers for visualization.
 * Written By: Tom Hicks. 1/15/2015.
 * Last Modified: Use local annotate method with no co-reference resolution.
 */
class DiscourseParserRunner (useProcessor:String = "core") {
  val processor:Processor =
    if (useProcessor == "fast")             // fast but slightly worse discourse parser
      new FastNLPProcessor(withDiscourse = true)
    else                                    // default: slow but better discourse parser
      new CoreNLPProcessor(withDiscourse = true)


  def parseText (text: String): DiscourseParserResults = {
    // create and annotate a document using the selected processor
    val doc = annotateNoCoRef(processor.mkDocument(text))
    // return information from discourse trees as an array of JSON strings:
    new DiscourseParserResults(text, discTrees(doc), synTrees(doc))
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


  def annotateNoCoRef (doc:Document): Document = {
    processor.tagPartsOfSpeech(doc)
    processor.lemmatize(doc)
    processor.recognizeNamedEntities(doc)
    processor.parse(doc)
    processor.chunking(doc)
    processor.labelSemanticRoles(doc)
    processor.discourse(doc)
    doc.clear()
    doc
  }

  override def toString:String = {
    return "<%s: %s>".format(super.toString(), useProcessor)
  }
}


class DiscourseParserResults(val text:String, val dTrees:Array[String], val synTrees:Array[String])
