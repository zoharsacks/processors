package edu.arizona.sista.processors.agiga

import java.io.{PrintWriter, FilenameFilter, File, Reader, FileOutputStream}
import edu.jhu.agiga.AgigaConstants.DependencyForm
import edu.jhu.agiga.AgigaConstants
import edu.jhu.agiga.StreamingDocumentReader
import edu.jhu.agiga.AgigaPrefs
import edu.jhu.agiga.{AgigaConstants, StreamingDocumentReader, AgigaPrefs}
import scala.collection.JavaConverters._
import scala.collection.mutable.{ListBuffer, ArrayBuffer}
import edu.arizona.sista.processors.Document
import edu.arizona.sista.processors.Sentence
import edu.arizona.sista.processors.{DocumentSerializer, Document, Sentence}
import edu.arizona.sista.struct.MutableNumber
import edu.arizona.sista.struct.DirectedGraph
import edu.arizona.sista.struct.Tree
import edu.arizona.sista.struct.{MutableNumber, DirectedGraph, Tree}
import edu.arizona.sista.utils.StringUtils
import org.slf4j.LoggerFactory
import scala.collection.parallel.ForkJoinTaskSupport
import edu.stanford.nlp.trees.SemanticHeadFinder
import edu.stanford.nlp.trees.{Tree => StanTree, SemanticHeadFinder}
import edu.arizona.sista.processors.corenlp.CoreNLPDocument
import edu.arizona.sista.processors.corenlp.{CoreNLPDocument, CoreNLPProcessor}
import edu.stanford.nlp.trees.TreeCoreAnnotations.TreeAnnotation
import edu.stanford.nlp.pipeline.Annotation
import edu.arizona.sista.processors.fastnlp.FastNLPProcessor
import edu.arizona.sista.processors.corenlp.CoreNLPProcessor
import edu.stanford.nlp.pipeline.Annotation
import edu.arizona.sista.utils.StringUtils
import edu.arizona.sista.processors.fastnlp.FastNLPProcessor
import edu.arizona.sista.processors.DocumentSerializer

/**
 * Created by gus on 4/18/14.
 */

class AgigaDiscourse {
  //enforce singelton pattern
}

object AgigaDiscourse {
  /** Convert agiga annotations to Document and then
    * process the document according to a specified view
    */
  //set par

  val logger = LoggerFactory.getLogger(classOf[AgigaDiscourse])

  val processor = new FastNLPProcessor(withDiscourse = true)

  lazy val headFinder = new SemanticHeadFinder()

  def mkTree(st: StanTree): Tree[String] = {
    val pos = new MutableNumber[Int](0)

    def toTree(stanfordTree: edu.stanford.nlp.trees.Tree, position: MutableNumber[Int]): Tree[String] = {
      assert(stanfordTree != null)

      if (stanfordTree.isLeaf) {
        val tree = new Tree[String](
          stanfordTree.label.value(),
          None, 0,
          position.value,
          position.value + 1)
        position.value += 1
        return tree
      }

      // println("Converting tree: " + stanfordTree.toString)
      val children = new Array[Tree[String]](stanfordTree.numChildren())
      for (i <- 0 until stanfordTree.numChildren()) {
        children(i) = toTree(stanfordTree.getChild(i), position)
      }
      val value = stanfordTree.label.value()
      val start = children(0).startOffset
      val end = children(children.length - 1).endOffset

      val headDaughter = headFinder.determineHead(stanfordTree)
      var head = -1
      var i = 0
      while (i < stanfordTree.numChildren() && head == -1) {
        if (headDaughter == stanfordTree.getChild(i)) {
          head = i
        }
        i += 1
      }

      new Tree[String](value, Some(children), head, start, end)
    }

    //calls the inner method, returns the tree made
    toTree(st, pos)
  }

  // label for agiga dependency type
  // val depForm = AgigaConstants.DependencyForm.COL_CCPROC_DEPS
  def createDocumentfromAgigaDoc(filename: String): Document = {
    // Setup Gigaword API Preferences
    val prefs = new AgigaPrefs()
    prefs.setAll(true)
    prefs.setWord(true)

    // Retrieve all gigaword documents contained within a given file
    val reader = new StreamingDocumentReader(filename, prefs)

    // For each document, count the frequency of terms, and add them to the document
    val docSentences: ArrayBuffer[Sentence] = new ArrayBuffer
    while (reader.hasNext) {

      val agigaDoc = reader.next()
      val sentences = agigaDoc.getSents.asScala
      for (s <- sentences) {

        // Constituency tree
        //val tree: Tree[String] = mkTree(s.getStanfordContituencyTree)

        // Sentence-essential stuff...
        val words = new ArrayBuffer[String]
        val lemmas = new ArrayBuffer[String]
        val posTags = new ArrayBuffer[String]
        val startOffsets = new ArrayBuffer[Int]
        val endOffsets = new ArrayBuffer[Int]

        // Sentence-optional stuff...
        val nerTags = new ArrayBuffer[String]

        // Collapsed dependencies...
        val collapsedDeps = s.getColCcprocDeps.asScala
        /** dir graph stuff: DirectedGraph[E](edges:List[(Int, Int, E)], val roots:collection.immutable.Set[Int]) */
        val edgeBuffer = new ListBuffer[(Int, Int, String)]
        val roots = new ListBuffer[Int]
        for (c <- collapsedDeps) {

          val depIndex = c.getDepIdx
          //val dep:String = s.getTokens.asScala(c.getDepIdx).getWord
          val headIndex = c.getGovIdx

          //root node
          if (headIndex == -1) {
            roots += depIndex
          } else {
            //is this equivalent to getShortname?
            val rel = c.getType
            edgeBuffer += ((headIndex, depIndex, rel))
            //println(s"dep: $depIndex head: $headIndex rel: $rel")
          }
        }
        //val tree = s.getStanfordContituencyTree.asScala

        //collect stuff for Sentence...
        for (w <- s.getTokens.asScala) {
          words += w.getWord
          lemmas += w.getLemma
          posTags += w.getPosTag
          nerTags += w.getNerTag
          startOffsets += w.getCharOffBegin

          endOffsets += w.getCharOffEnd
        }
        val deps = new DirectedGraph[String](edgeBuffer.toList, roots.toSet)
        // perhaps one of the above are empty...

        //testing the indexing of character offsets -- they are zero indexed, the offsets are def character offsets,
        // not word offsets...
//        for (i <- 0 until startOffsets.size){
//          println (s"i:$i\toffset: ${startOffsets(i)}")
//        }
        val sent = new Sentence(
          /** Actual tokens in this sentence */
          words.toArray,

          /** Start character offsets for the words; start at 0 */
          //TODO: make sure this is zero-indexed...
          startOffsets.toArray,

          /** End character offsets for the words; start at 0 */
          //TODO: make sure this is zero-indexed...
          endOffsets.toArray,

          /** POS tags for words (OPTION) */
          Some(posTags.toArray),

          /** Lemmas (OPTION) */
          Some(lemmas.toArray),

          /** NE labels (OPTION) */
          Some(nerTags.toArray),

          /** Normalized values of named/numeric entities, such as dates (OPTION) */
          None,

          /** Shallow parsing labels (OPTION) */
          None,

          /** Constituent tree of this sentence; includes head words Option[Tree[String]] */
          //Some(tree),
          None,

          /** *Dependencies: Option[DirectedGraph[String]]) */
          Some(deps))

        docSentences += sent
        //println(s"Words: ${words.mkString(" ")}")
      }
    }
    new CoreNLPDocument(docSentences.toArray, None, None, Some(new Annotation("")))
    //new Document(docSentences.toArray)
  }


  def findFiles(collectionDir: String): Array[File] = {
    val dir = new File(collectionDir)
    dir.listFiles(new FilenameFilter {
      def accept(dir: File, name: String): Boolean = name.endsWith(".xml.gz")
    })
  }

  def fileExists(file: File, outFolder: String, desiredView: String): Boolean = new File(outFolder, s"${file.getName}-$desiredView").exists

  def processDoc(doc: Document) {
    processor.discourse(doc)
  }

  def main(args: Array[String]) {
    val props = StringUtils.argsToProperties(args)
    //val processor = new CoreNLPProcessor(withDiscourse = true)
    val ds = new DocumentSerializer
    // test whether input is Directory or File
    val defaultInputFolder: String = "/data1/nlp/corpora/agiga/data/xml"
    val inFolder = props.getProperty("agiga.inputfolder", defaultInputFolder)

    val defaultOutputFile: String = s"/data1/nlp/corpora/dependencies/gigaword-discourse"
    val outFile = props.getProperty("agiga.doc_outputfile", defaultOutputFile)
    val os = new PrintWriter(outFile)

    // determine nature of input (file vs directory)
    val dataLoc = new File(inFolder)

    //I think I can just have:
    val files: Array[File] = if (dataLoc.isDirectory) findFiles(inFolder) else Array(dataLoc)

    // set max threads
    //files.tasksupport = new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(8))

    logger.info(s"Input: $inFolder")
    logger.info(s"Output file: $outFile")
    for (file <- files) {
      logger.info(s"Creating Document from ${file.getAbsolutePath}")
      val doc: Document = createDocumentfromAgigaDoc(file.getAbsolutePath)
      logger.info(s"Processing ${file.getAbsolutePath}")
      //make the discourse tree, modifies the doc in place
      processDoc(doc)

      //      for (s <- doc.sentences) {
      //        println(s"Sentence: ${s.getSentenceText()}")
      //
      //        s.syntacticTree.foreach(tree => {
      //          for (child <- tree.children.get) {
      //            println(s"Head [offset = ${child.head}] [head tree = ${child.children.get(child.head)}}]  ")
      //          }
      //          println("Constituent tree: " + tree)
      //        })
      //
      //      }
      println(s"Discourse Tree made!")
      //println(s"Discourse Tree: ${doc.discourseTree}")
      ds.save(doc, os)

      logger.info(s"Document from ${file.getAbsolutePath} complete and written to $outFile!")

    }

    os.close()
  }
}